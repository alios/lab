{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Geodetic (
  GeodeticModel (..),
  ANS, ans, ans',
  WGS84, wgs84, wgs84',
  GRS80, grs80, grs80',
  GeodeticCoordinate (..), refElipsoid, latitude, longitude, height,
  ECEF (..), coordX, coordY, coordZ,
  gcDist,
  CourseDirection (..),
  gcCourse,
  dr
  ) where

import qualified Prelude ()
import Numeric.Units.Dimensional.TF.Prelude
import Control.Lens hiding (_1, _2, _3, (*~))
import Data.Typeable
import Data.Fixed

data GeodeticCoordinate m t =
  GeodeticCoordinate {
    _refElipsoid :: !m,
    _latitude :: !(PlaneAngle t),
    _longitude :: !(PlaneAngle t),
    _height :: !(Length t)
    } deriving (Show, Eq, Typeable)
makeLenses ''GeodeticCoordinate

data ECEF t =
  ECEF {
    _coordX :: !(Length t),
    _coordY :: !(Length t),
    _coordZ :: !(Length t)
    } deriving (Show, Eq, Typeable)
makeLenses ''ECEF

data CourseDirection = CourseEast | CourseWest
                     deriving (Show, Eq, Typeable)
               
class (Eq m, Typeable m) => GeodeticModel m where
  semiMajorAxis :: (Fractional t) => m -> Length t
  recProcFlattening :: (Fractional t) => m -> Dimensionless t
  flattening :: (Fractional t) => m -> Dimensionless t
  flattening m = _1 / (recProcFlattening m)
  semiMinorAxis :: (Fractional t) => m -> Length t
  semiMinorAxis m = (semiMajorAxis m) * (_1 - flattening m)
  fstEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  fstEccentricity m =
    let f = flattening m
    in (_2 * f) - (f ** _2)
  sndEccentricity :: (Floating t, Fractional t) => m -> Dimensionless t
  sndEccentricity m =     
    let f = flattening m
        a = f * (_2 - f) 
        b = ((_1 - f) ** _2)
    in  a / b
  toEcef :: (Floating t) => GeodeticCoordinate m t -> ECEF t
  toEcef c =
    let φ = c ^. longitude
        λ = c ^. latitude
        h = c ^. height
        e2 = fstEccentricity $ _refElipsoid c
        x = sqrt (_1 - (e2 * ((sin φ) ** _2)))
        a = semiMajorAxis $ _refElipsoid c
        normal = a / x
        normalh = normal + h
        rx = normalh * (cos φ) * (cos λ)
        ry = normalh * (cos φ) * (sin λ)
        rz = (((a * ( _1 - e2)) / x) + h) * (sin φ)
    in ECEF rx ry rz
  fromEcef :: (RealFloat t) => m -> ECEF t -> GeodeticCoordinate m t
  fromEcef m coord = 
    let x = coord ^. coordX
        y = coord ^. coordY
        z = coord ^. coordZ 
        a = semiMajorAxis m
        b = semiMinorAxis m
        e2 = fstEccentricity m
        e'2 = sndEccentricity m
        r = sqrt ((x * x) + (y * y))
        ee2 = (a * a) - (b * b)
        f = (54 *~ one) * (b * b) * (z * z)
        g = (r * r) + ((_1 - e2) * z * z) - (e2 * e2 * ee2)
        c = (e2 * e2 * f * r * r ) / ( g * g * g )
        s = cbrt (_1 + c + sqrt ((c*c) + (_2 * c)))  
        p = f / (_3 * ((s + (_1 / s) + _1) ** _2) * (g * g))
        q = sqrt (_1 + (_2 * e2 * e2 * p))
        r0a = ((_0 - _1) * (p * e2 * r)) / (_1 + q) 
        r0b = sqrt ((((_1 / _2) * a * a) * (_1 + (_1 / q)))- 
                    ((p * (_1 - e2) * z * z)/(q * (_1 + q))) - 
                    ((_1 / _2) * p * r * r))
        r0 = r0a + r0b
        ub = z * z
        ua = (r  - (e2 * r0)) * (r  - (e2 * r0))
        u = sqrt (ua + ub) 
        v = sqrt (ua + ((_1 - e2) * ub))
        z0 = (b * b * z) / (a * v)
        h = u * (_1 - ((b * b)/(a * v)))
        φ = atan ((z + (e'2 * z0)) / r)
        λ = atan2 y x
    in GeodeticCoordinate m λ φ h
  geodeticModel :: m
  fromGeodetic :: (GeodeticModel a, Typeable t, RealFloat t) =>
                  GeodeticCoordinate a t -> GeodeticCoordinate m t
  fromGeodetic = fromEcef geodeticModel . toEcef 

sm :: (Fractional t) => Length t
sm = 1.852 *~ kilo meter


-- | greate circle distance
gcDist :: (GeodeticModel m, Floating t) => GeodeticCoordinate m t -> GeodeticCoordinate m t -> Length t
gcDist c1 c2 = c1 `gcDist'` c2
    where gcDist' (GeodeticCoordinate _ φ1 λ1 _) (GeodeticCoordinate _ φ2 λ2 _) =
            let dλ = abs $ λ1 - λ2
                a1 = (cos φ1 * sin dλ)
                a2 = (cos φ2 * sin φ1) - (sin φ2 * cos φ1 * cos dλ)
                a = sqrt ((a1 * a1) + (a2 *a2))     
                b = (sin φ2 * sin φ1) + (cos φ2 * cos φ1 * cos dλ)      
                dσ = atan (a / b)
                r = 6371.01 *~ kilo meter
            in r * dσ

-- | greate circle course
gcCourse :: (Ord t, Real t, Floating t, GeodeticModel a) =>
            CourseDirection -> GeodeticCoordinate a t -> GeodeticCoordinate a t -> PlaneAngle t
gcCourse d a b =
  let jb = b ^. longitude
      dg = gcDist a b / (1 *~ meter)
      ja = a ^. longitude 
      ar = ((sin jb) - (cos dg) * (sin ja)) / ((cos ja) * (sin dg))
      d360 = (360 *~ degree)
      c = case d of
        CourseWest -> ar
        CourseEast -> d360 - ar
  in ((c /~ degree) `mod'` 360) *~ degree
  

        
data ANS = ANS deriving (Show, Eq, Typeable)
instance GeodeticModel ANS where
  geodeticModel = ANS
  semiMajorAxis _ = (6378160.0 *~ meter)
  recProcFlattening _ = (298.25 *~ one)

ans' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate ANS t
ans' φ λ h = GeodeticCoordinate ANS φ λ h

ans :: (Floating t) => t -> t -> t -> GeodeticCoordinate ANS t
ans φ λ h = ans' (φ *~ degree) (λ *~ degree) (h *~ meter)


data WGS84 = WGS84 deriving (Show, Eq, Typeable)
instance GeodeticModel WGS84 where
  geodeticModel = WGS84
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257223563 *~ one)

wgs84' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate WGS84 t
wgs84' φ λ h = GeodeticCoordinate WGS84 φ λ h

wgs84 :: (Floating t) => t -> t -> t -> GeodeticCoordinate WGS84 t
wgs84 φ λ h = wgs84'(φ *~ degree) (λ *~ degree) (h *~ meter)


data GRS80 = GRS80 deriving (Show, Eq, Typeable)
instance GeodeticModel GRS80  where
  geodeticModel = GRS80
  semiMajorAxis _ = (6378137.0 *~ meter)
  recProcFlattening _ = (298.257222101 *~ one)

grs80' :: (Floating t) => PlaneAngle t -> PlaneAngle t -> Length t -> GeodeticCoordinate GRS80 t
grs80' φ λ h = GeodeticCoordinate GRS80 φ λ h

grs80 :: (Floating t) => t -> t -> t -> GeodeticCoordinate GRS80 t
grs80 φ λ h = grs80'(φ *~ degree) (λ *~ degree) (h *~ meter)



dr :: (Floating t) =>
      GeodeticCoordinate m t -> Velocity t -> Time t -> PlaneAngle t -> PlaneAngle t --GeodeticCoordinate m t 
dr fix v t a =
  let l = ((v * t) / sm)
  in l

t = dr ms (160 *~ (kilo meter / hour)) (30 *~ minute) (90 *~ degree)



msgrs :: GeodeticCoordinate GRS80 Double
msgrs = fromGeodetic ms



cmsny = gcCourse CourseWest ms ny
cmshh = gcCourse CourseEast ms hh


ms = wgs84 51.969659 7.605286 0

foo = wgs84 51.969659 23.605286 0

mse = toEcef ms
ms2 = fromEcef WGS84 mse

hh = wgs84 53.543572 10.02502 74
hhg = grs80 53.543572 10.02502 74

fa = gcDist ms hh
fb = gcDist ms $ fromGeodetic hhg

ny = wgs84 40.43 (-74) 0


d1 = ms ^. latitude - ms2 ^.latitude
d2 = ms ^. longitude - ms2 ^.longitude
d3 = ms ^. height - ms2 ^.height


xx = [ wgs84 x y 0 | x <- [0..359] , y <- [-90, 90] ]
xxx = fmap (fromEcef WGS84 . toEcef) xx

xxxx = zip xx xxx

xd f =
  let ds = map (\(a,b) -> a ^. f - b ^. f) xxxx
  in (minimum ds, maximum ds)
xd1 = xd latitude     
xd2 = xd longitude
xd3 = xd height


