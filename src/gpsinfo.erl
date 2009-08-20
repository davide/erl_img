%%% File    : gpsinfo.erl
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Description : Utils for decoding Exif GPSInfo tags
%%% Created :  20 Aug 2009 by Davide Marquês <nesrai@gmail.com>

-module(gpsinfo).

-export([decode_tag/1, collect_gpsinfo/3]).

-include("gpsinfo.hrl").
-include("tiff.hrl").
-include("dbg.hrl").

collect_gpsinfo(_Fd, T, St) ->
    _Key = decode_tag(T#tiff_entry.tag),
    ?dbg("GPS_INFO(~s) ~p ~p ~p\n", 
	 [T#tiff_entry.ifd,_Key,T#tiff_entry.type,T#tiff_entry.value]),
    case T#tiff_entry.tag of
	_ ->
	    St
    end.


decode_tag(Tag) when is_integer(Tag) ->
    case Tag of
	?GPSVersionID -> 'GPSVersionID';
	?GPSLatitudeRef -> 'GPSLatitudeRef';
	?GPSLatitude -> 'GPSLatitude';
	?GPSLongitudeRef -> 'GPSLongitudeRef';
	?GPSLongitude -> 'GPSLongitude';
	?GPSAltitudeRef -> 'GPSAltitudeRef';
	?GPSAltitude -> 'GPSAltitude';
	?GPSTimeStamp -> 'GPSTimeStamp';
	?GPSSatellites -> 'GPSSatellites';
	?GPSStatus -> 'GPSStatus';
	?GPSMeasureMode -> 'GPSMeasureMode';
	?GPSDOP -> 'GPSDOP';
	?GPSSpeedRef -> 'GPSSpeedRef';
	?GPSSpeed -> 'GPSSpeed';
	?GPSTrackRef -> 'GPSTrackRef';
	?GPSTrack -> 'GPSTrack';
	?GPSImgDirectionRef -> 'GPSImgDirectionRef';
	?GPSImgDirection -> 'GPSImgDirection';
	?GPSMapDatum -> 'GPSMapDatum';
	?GPSDestLatitudeRef -> 'GPSDestLatitudeRef';
	?GPSDestLatitude -> 'GPSDestLatitude';
	?GPSDestLongitudeRef -> 'GPSDestLongitudeRef';
	?GPSDestLongitude -> 'GPSDestLongitude';
	?GPSDestBearingRef -> 'GPSDestBearingRef';
	?GPSDestBearing -> 'GPSDestBearing';
	?GPSDestDistanceRef -> 'GPSDestDistanceRef';
	?GPSDestDistance -> 'GPSDestDistance';
	?GPSProcessingMethod -> 'GPSProcessingMethod';
	?GPSAreaInformation -> 'GPSAreaInformation';
	?GPSDateStamp -> 'GPSDateStamp';
	?GPSDifferential -> 'GPSDifferential';
	Tag -> Tag
    end;
decode_tag(Tag) ->
    Tag.
