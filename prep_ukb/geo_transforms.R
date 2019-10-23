#################################################################################
# Functions to transform OSG36 easting/northing coords to WGS84 lat/lon         #
# Adapted from https://www.movable-type.co.uk/scripts/latlong-os-gridref.html   #
# And the associated Git Repo https://github.com/chrisveness/geodesy            #
#################################################################################


# function usage:
# OSGtoLatLon(easting, northing, newdatum)
# output is lat/lon list of coords


# set ellipsoids
ellipsoids = list(
      WGS84 =         data.frame( a = 6378137,     b = 6356752.314245, f = 1/298.257223563 )
    , Airy1830 =      data.frame( a = 6377563.396, b = 6356256.909,    f = 1/299.3249646   )
    , AiryModified =  data.frame( a = 6377340.189, b = 6356034.448,    f = 1/299.3249646   )
    , Bessel1841 =    data.frame( a = 6377397.155, b = 6356078.962818, f = 1/299.1528128   )
    , Clarke1866 =    data.frame( a = 6378206.4,   b = 6356583.8,      f = 1/294.978698214 )
    , Clarke1880IGN = data.frame( a = 6378249.2,   b = 6356515.0,      f = 1/293.466021294 )
    , GRS80 =         data.frame( a = 6378137,     b = 6356752.314140, f = 1/298.257222101 )
    , Intl1924 =      data.frame( a = 6378388,     b = 6356911.946,    f = 1/297           )
    , WGS72 =         data.frame( a = 6378135,     b = 6356750.5,      f = 1/298.26        )
)


### Set Datum constants
datums = list(
# transforms: t in metres, s in ppm, r in arcseconds              tx       ty        tz       s        rx        ry        rz
     'ED50' =       list(ellipsoid = 'Intl1924',      transform = c(   89.5,    93.8,    123.1,    -1.2,     0.0,      0.0,      0.156    )) # epsg.io/1311
    , 'ETRS89' =     list(ellipsoid = 'GRS80',         transform = c(    0,       0,        0,       0,       0,        0,        0        )) # epsg.io/1149; @ 1-metre level
    , 'Irl1975' =    list(ellipsoid = 'AiryModified',  transform = c( -482.530, 130.596, -564.557,  -8.150,   1.042,    0.214,    0.631    )) # epsg.io/1954
    , 'NAD27' =      list(ellipsoid = 'Clarke1866',    transform = c(    8,    -160,     -176,       0,       0,        0,        0        ))
    , 'NAD83' =      list(ellipsoid = 'GRS80',         transform = c(    0.9956, -1.9103,  -0.5215, -0.00062, 0.025915, 0.009426, 0.011599 ))
    , 'NTF' =        list(ellipsoid = 'Clarke1880IGN', transform = c(  168,      60,     -320,       0,       0,        0,        0        ))
    , 'OSGB36' =     list(ellipsoid = 'Airy1830',      transform = c( -446.448, 125.157, -542.060,  20.4894, -0.1502,  -0.2470,  -0.8421   )) # epsg.io/1314
    , 'Potsdam' =    list(ellipsoid = 'Bessel1841',    transform = c( -582,    -105,     -414,      -8.3,     1.04,     0.35,    -3.08     ))
    , 'TokyoJapan' = list(ellipsoid = 'Bessel1841',    transform = c(  148,    -507,     -685,       0,       0,        0,        0        ))
    , 'WGS72' =      list(ellipsoid = 'WGS72',         transform = c(    0,       0,       -4.5,    -0.22,    0,        0,        0.554    ))
    , 'WGS84' =      list(ellipsoid = 'WGS84',         transform = c(    0.0,     0.0,      0.0,     0.0,     0.0,      0.0,      0.0      ))
)


# to transform between coord systems:
#   1. transform lat/lon to cartesian
#   2. [If not in WGS84] transform to WGS84
#   3. Transform from WGS84 -> new datum (IN CARTESIAN COORDS)
#   4. Transform new cartesian coords to lat/lon


# transformation from one cartesian system to another
applyTransform = function(xyz, t){

    x1 = xyz$x
    y1 = xyz$y
    z1 = xyz$z
    # transform parameters
    tx = t[1]                    # x-shift in metres
    ty = t[2]                    # y-shift in metres
    tz = t[3]                    # z-shift in metres
    s  = t[4]/1e6 + 1            # scale: normalise parts-per-million to (s+1)
    rx = deg2rad(t[5]/3600)       # x-rotation: normalise arcseconds to radians
    ry = deg2rad(t[6]/3600)       # y-rotation: normalise arcseconds to radians
    rz = deg2rad(t[7]/3600)       # z-rotation: normalise arcseconds to radians

        # apply transform
    x2 = tx + x1 * s  - y1 * rz + z1 * ry
    y2 = ty + x1 * rz + y1 * s  - z1 * rx
    z2 = tz - x1 * ry + y1 * rx + z1 * s

    return(data.frame(x = x2, y = y2, z = z2))
    }
  

LatLonToCart = function(lat, lon, height = 0, orig_ellipsoid = 'WGS84') {
    # x = (nu+h)⋅cosphi⋅coslambda, y = (nu+h)⋅cosphi⋅sinlambda, z = (nu⋅(1-e²)+h)⋅sinphi
    # where nu = a/√(1−e²⋅sinphi⋅sinphi), e² = (a²-b²)/a² or (better conditioned) 2⋅f-f²

    phi = deg2rad(lat)
    lambda = deg2rad(lon)
    h = height

    # get constants based on the ellipsoid we're transforming
    a = ellipsoids[[orig_ellipsoid]]$a
    b = ellipsoids[[orig_ellipsoid]]$b
    f = ellipsoids[[orig_ellipsoid]]$f

    sinphi = sin(phi)
    cosphi = cos(phi)
    sinlambda = sin(lambda)
    coslambda = cos(lambda)

    eSq = 2*f - f*f                      # 1st eccentricity squared ≡ (a²-b²)/a²
    nu = a / sqrt(1 - eSq*sinphi*sinphi) # radius of curvature in prime vertical

    x = (nu+h) * cosphi * coslambda
    y = (nu+h) * cosphi * sinlambda
    z = (nu*(1-eSq)+h) * sinphi

    return(data.frame(x, y, z))
    }

CartToLatLon = function(xyz, ellipsoid) {

    x = xyz$x
    y = xyz$y
    z = xyz$z

    # get constants based on the ellipsoid we're transforming
    a = ellipsoids[[ellipsoid]]$a
    b = ellipsoids[[ellipsoid]]$b
    f = ellipsoids[[ellipsoid]]$f

    e2 = 2*f - f*f              # 1st eccentricity squared ≡ (a²−b²)/a²
    epsilon2 = e2 / (1-e2)      # 2nd eccentricity squared ≡ (a²−b²)/b²
    p = sqrt(x*x + y*y)         # distance from minor axis
    R = sqrt(p*p + z*z)         # polar radius

    # parametric latitude (Bowring eqn.17, replacing tanbeta = z·a / p·b)
    tanbeta = (b*z)/(a*p) * (1+epsilon2*b/R)
    sinbeta = tanbeta / sqrt(1+tanbeta*tanbeta)
    cosbeta = sinbeta / tanbeta

    # geodetic latitude (Bowring eqn.18: tanphi = z+epsilon²⋅b⋅sin³beta / p−e²⋅cos³beta)
    phi = ifelse(is.nan(cosbeta), 0, atan2(z + epsilon2*b*sinbeta*sinbeta*sinbeta, p - e2*a*cosbeta*cosbeta*cosbeta))

    # longitude
    lambda = atan2(y, x)

    # height above ellipsoid (Bowring eqn.7)
    sinphi = sin(phi)
    cosphi = cos(phi)
    nu = a / sqrt(1-e2*sinphi*sinphi) # length of the normal terminated by the minor axis
    h = p*cosphi + z*sinphi - (a*a/nu)

    latlon = list(lat = rad2deg(phi), lon = rad2deg(lambda))

    return(latlon)
}
 
deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad * 180) / (pi)}

OSGtoLatLon = function(easting, northing, newdatum = 'WGS84'){
	 a = 6377563.396
	 b = 6356256.909      				# Airy 1830 major & minor semi-axes
     F0 = 0.9996012717                          # NatGrid scale factor on central meridian
     phi0 = deg2rad(49)
     lambda0 = deg2rad(-2)					# NatGrid true origin is 49°N,2°W
     N0 = -100e3
     E0 = 400e3                     		# northing & easting of true origin, metres
     e2 = 1 - (b*b)/(a*a)                   # eccentricity squared
     n = (a-b)/(a+b)
     n2 = n*n
     n3 = n*n*n        						# n, n², n³

     phi = phi0
     M = 0
     while(abs(northing - N0 - M) >= 0.00001){
     	phi = (northing - N0 - M)/(a*F0) + phi;

        Ma = (1 + n + (5 / 4)*n2 + (5 / 4)*n3) * (phi-phi0)
        Mb = (3 * n + 3 * n * n + (21 / 8) * n3) * sin(phi-phi0) * cos(phi+phi0)
        Mc = ((15 / 8) * n2 + (15 / 8) * n3) * sin(2 * (phi-phi0)) * cos(2 * (phi+phi0))
        Md = (35 / 24) * n3  *  sin(3 * (phi - phi0)) * cos(3 * (phi + phi0))
        M = b * F0 * (Ma - Mb + Mc - Md)               # meridional arc
     }


     cosphi = cos(phi)
     sinphi = sin(phi)
     nu = a * F0 / sqrt(1-e2 * sinphi * sinphi)             	# nu = transverse radius of curvature
     rho = a * F0 * (1-e2) / `^`(1-e2 * sinphi * sinphi, 1.5)     # rho = meridional radius of curvature
     eta2 = nu / rho-1                                   		# eta = ?

     tanphi = tan(phi)
     tan2phi = tanphi * tanphi
     tan4phi = tan2phi * tan2phi
     tan6phi = tan4phi * tan2phi

     secphi = 1 / cosphi
     nu3 = nu * nu * nu
     nu5 = nu3 * nu * nu
     nu7 = nu5 * nu * nu
     VII = tanphi / (2 * rho * nu)
     VIII = tanphi / (24 * rho * nu3) * (5 + 3 * tan2phi + eta2 - 9 * tan2phi * eta2)
     IX = tanphi / (720 * rho * nu5) * (61 + 90 * tan2phi + 45 * tan4phi)
     X = secphi / nu
     XI = secphi / (6 * nu3) * (nu / rho + 2 * tan2phi)
     XII = secphi / (120 * nu5) * (5 + 28 * tan2phi+24 * tan4phi)
     XIIA = secphi / (5040 * nu7) * (61 + 662 * tan2phi + 1320 * tan4phi + 720 * tan6phi)

     dE = (easting - E0)
     dE2 = dE * dE
     dE3 = dE2 * dE
     dE4 = dE2 * dE2
     dE5 = dE3 * dE2
     dE6 = dE4 * dE2
     dE7 = dE5 * dE2
     phi = phi - VII * dE2 + VIII * dE4 - IX * dE6
     lambda = lambda0 + X * dE - XI * dE3 + XII * dE5 - XIIA * dE7

     # point in OSGB36
     point = list(lat = rad2deg(phi), lon = rad2deg(lambda))

     if(newdatum != 'OSGB36'){
        # to cartesian
        xyz = LatLonToCart(lat = point$lat, lon = point$lon, orig_ellipsoid = datums$OSGB36$ellipsoid)

        # first transform to WGS84
        xyz_new = applyTransform(xyz, t = -datums$OSGB36$transform) #negative transform

        # apply another transform if necessary
        if(newdatum != 'WGS84'){
            xyz_new = applyTransform(xyz_new, t = datums[[newdatum]]$transform) 
        }

        latlon_out = CartToLatLon(xyz_new, ellipsoid = datums[[newdatum]]$ellipsoid)
        
     }else{
        latlon_out = point
     }

        # if (datum != LatLonEllipsoidal.datums.OSGB36) {
        #     # if point is required in datum other than OSGB36, convert it
        #     point = point.convertDatum(datum);
        #     # convertDatum() gives us a LatLon: convert to LatLon_OsGridRef which includes toOsGrid()
        #     point = LatLon_OsGridRef(point.lat, point.lon, point.height, point.datum);
        # }

    return(latlon_out)
}
