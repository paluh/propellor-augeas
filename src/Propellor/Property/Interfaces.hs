module Propellor.Property.Interfaces where

import           Data.Optional (Optional(..))

data Start = Auto | AllowAuto | AllowHotPlug

iPv4Address :: String -> Maybe IPAddress
iPv4Address raw = Just . IPv4 $ raw

data IPAddress = IPv4 String | IPv6 String
type Broadcast = IPAddress
type Gateway = IPAddress
type Network = IPAddress
type Netmask = IPAddress

data Asquisition =
  DHCP | Manual | PPP | Static IPAddress Gateway Network Netmask Broadcast

data Bond =
  Bond {
    bondMode :: Int
  , bondMiimon :: Int
  , bondDowndelay :: Int
  , bondUpdelay :: Int
  }

data Interface =
  Interface {
      start :: Optional Start
    , asquisition :: Asquisition
    , dnsNameservers :: [IPAddress ]
    , bond :: Optional Bond
    }
