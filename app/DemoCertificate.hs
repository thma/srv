{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module DemoCertificate
  ( demoTLSSettings,
  )
where

{--
This module provides a self signed certificate for testing the https server.
It was generated with the following commands:

openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem

--}

import qualified Data.ByteString             as S
import           Network.Wai.Handler.WarpTLS (TLSSettings (..), tlsSettingsMemory)
import           Text.RawString.QQ           (r)

demoTLSSettings :: TLSSettings
demoTLSSettings = tlsSettingsMemory certificate key

certificate :: S.ByteString
certificate =
  [r|-----BEGIN CERTIFICATE-----
MIIDczCCAlsCFGVNzDOVmV85+d//lhqENnblNF8EMA0GCSqGSIb3DQEBCwUAMHYx
CzAJBgNVBAYTAkRFMQwwCgYDVQQIDANOUlcxDjAMBgNVBAcMBUVzc2VuMQ4wDAYD
VQQKDAVNaWVrZTEMMAoGA1UECwwDQ0VPMQ4wDAYDVQQDDAVNaWVrZTEbMBkGCSqG
SIb3DQEJARYMbWlla2VAd2ViLmRlMB4XDTIyMDYyMTE5MzYxM1oXDTIyMDcyMTE5
MzYxM1owdjELMAkGA1UEBhMCREUxDDAKBgNVBAgMA05SVzEOMAwGA1UEBwwFRXNz
ZW4xDjAMBgNVBAoMBU1pZWtlMQwwCgYDVQQLDANDRU8xDjAMBgNVBAMMBU1pZWtl
MRswGQYJKoZIhvcNAQkBFgxtaWVrZUB3ZWIuZGUwggEiMA0GCSqGSIb3DQEBAQUA
A4IBDwAwggEKAoIBAQDhW96hp1iCG41M22rTKiLeE40VbRsFjJpgzgMLBUAZYWik
vFdpb1A+cU2qiTfNf7gGK/U8gmJsdTEk+x34Pq6xt7/naFFFyBFBE689lYA7qf7m
w76stvivrG/QIGR3eLpP5MuDr3tlYOkz6KJDso9lxvMF2qswJWoPemSJbtd8tUlN
9/vzHvlllihjeTwKJt5ha1Eq8v21V2ZqTHPZP8858C+it/4+zihRrc/UpWN3LWHH
Skv9xK+3X7s76H1+0mtji/bZuzS0+oPCrDKMQwPv2mn9qT4c9nV2E0lH7eiI8trU
OYi+cvGPSl3w5+JTnHzoQfOhToPeu0+ll7Q+XOyRAgMBAAEwDQYJKoZIhvcNAQEL
BQADggEBAE5WkrlmVWMqa5vPYjC6fo7344hviRODPEUM+Ju2lfe0hdl2CpEsAsHr
U/5Ho/XEvo7GBov0WxypPCLY0eWDynrodxpVfg7KWFtmQu7FN4PHQcDgcIXXENHE
FiWYmyDqLhR3v8Cg+grDHFDVrrjxki0ylcKKIKPN3p1NPfZAzG44kDtJYAC+Fjzr
RgIWrVcmHnukQojG/gnt6J6T7Fk1OyswosnspI/ct7xblcR7h3iycr3rbxXvcDgA
GpwyIkl8Fyp4y/1IqebAuiRYLR+Pw00i19TJQvqnenEjrCpYeK3T83Zstke/pHn6
Z1c13pIiIUAEFs56zT6aNh5qPuAyHQ0=
-----END CERTIFICATE-----|]

key :: S.ByteString
key =
  [r|-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEA4VveoadYghuNTNtq0yoi3hONFW0bBYyaYM4DCwVAGWFopLxX
aW9QPnFNqok3zX+4Biv1PIJibHUxJPsd+D6usbe/52hRRcgRQROvPZWAO6n+5sO+
rLb4r6xv0CBkd3i6T+TLg697ZWDpM+iiQ7KPZcbzBdqrMCVqD3pkiW7XfLVJTff7
8x75ZZYoY3k8CibeYWtRKvL9tVdmakxz2T/POfAvorf+Ps4oUa3P1KVjdy1hx0pL
/cSvt1+7O+h9ftJrY4v22bs0tPqDwqwyjEMD79pp/ak+HPZ1dhNJR+3oiPLa1DmI
vnLxj0pd8OfiU5x86EHzoU6D3rtPpZe0PlzskQIDAQABAoIBAQCHqyUGMJaqDSgt
otJWucEv7Jk80ug3mQO/T4apdcm2/dtfgTsmcCrgzJkzF1SO1FrW7FPAz8WtAYhK
eRr7vdCxkKtpBnYXCnDIuq7+5ifCw35/Mkxx8d0Y9TiDFV0mR3nyawzSBCzHRWZf
J6e4FaEaZoEbfx8b5wpIsygfTKlRmzh9B6VNc6xWDCrVPjBuXb8+5Hq+HTmEau9J
e/87z2VMhR+i+UnefjrO4/ewtvyk8PRYsFYzJ7zqd2k0MfC1M7vBJEn1ei1NuqA5
9HPkD+N/CkwbOqnpDFOVjbCZLEfP2CiXu/wmRW6zzp7Bp0iqjtAAyq1Yd84SeyUv
XCq+uuAZAoGBAPSPGz7wr7ECsGLreDF6lfoyADh2pNqKdSDLo6KPN8kStmA/sxUs
T0T3goKxbocd4JeRdQ0IIMZ1MRhilPYAVR6hWF2H2BVNg1O9o/yUDjHug3G661rg
5dUI7m7aWgjYPDCVJMJqRTs+WmqZgVT+Xw6m4LvAbARoq7B2B+JAW08TAoGBAOvm
0Wg5Be+L8OMWH7VWGiCJqcP1sn3x5fjsEp2QRcMWgi69/vLOeAcY0wBIUxPC/QYn
bIcTqCQpEbz5dpZvtKQgxAOrRdkA+Pw2DifE7A91fIf4/NhmjPGMFg7tjW5E8Efa
vPjfhE1jUTyLLdpXIAIEAadbjNhkQu+C0HjBbnZLAoGBAJRhW4OVntOEMSbjfTpw
CR0TR0T630zlYulyBKd1w8AQxWyiWXKIPtQ2ZA4Nv2TlgMYVb/JnMxOWOSJ1dbHI
Q9zgwe+Fo1pRfFPLpPpzjiVHdKMFaRTrjSbzU08Y1jW1lNdZNkNrHPQ0AcHZvzml
WGFYaNQhjKgkJb9b7a6do4jXAoGBAJpyuM/f5b78hG5pYcE76kc7WmbuSjqjUcoz
5cw7VTFxXzJzOqSGAZfD7QHWWG4rz7Vjwm59tCxYp0Buo/HCmSaMPUmZrVVyiZG3
YnUvPM6yQSJVJlYYblcHPLnxVhPIFvk0gV0AGoMAb4OQLCkzucUYEm0cR461tg1S
PGOTYVsZAoGAVQ7k7jnWC/91FikRR3/MuG02zaEAcd9VeioDguuOqyPAIxqQjV+N
d9vA3TBVpgYaUqImnc4eJST4g8oRNK0fPMwKxlUnuJv0dKb/iA3UDfdD59mHbENv
VFEmMo/MDbeBYV758HP6+EMNaAYl1LjrlyiI7ypaUrcc+mPiwVA5vn8=
-----END RSA PRIVATE KEY-----|]
