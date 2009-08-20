%%% File    : gpsinfo.erl
%%% Author  : Davide Marquês <nesrait@gmail.com>
%%% Description : Utils for decoding Exif GPSInfo tags
%%% Created :  20 Aug 2009 by Davide Marquês <nesrai@gmail.com>

-module(gpsinfo).

-export([decode_tag/1, collect_gpsinfo/3]).
-export([latitude/1, longitude/1]).

-include("gpsinfo.hrl").
-include("tiff.hrl").
-include("dbg.hrl").

collect_gpsinfo(_Fd, T, Info) ->
    Key = decode_tag(T#tiff_entry.tag),
    ?dbg("GPS_INFO(~s) ~p ~p ~p\n", 
	 [T#tiff_entry.ifd,Key,T#tiff_entry.type,T#tiff_entry.value]),
    [{Key,T#tiff_entry.value} | Info].

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

latitude(GPSInfo) ->
    case proplists:get_value('GPSLatitudeRef', GPSInfo) of
	undefined ->
	    undefined;
	Ref ->
	    case proplists:get_value('GPSLatitude', GPSInfo) of
		undefined ->
		    undefined;
		Sexagesimal ->
		    Dec = sexagesimal_to_decimal(Sexagesimal),
		    case Ref of
			["N"] -> Dec;
			["S"] -> -Dec
		    end
	    end
    end.

longitude(GPSInfo) ->
    case proplists:get_value('GPSLongitudeRef', GPSInfo) of
	undefined ->
	    undefined;
	Ref ->
	    case proplists:get_value('GPSLongitude', GPSInfo) of
		undefined ->
		    undefined;
		Sexagesimal ->
		    Dec = sexagesimal_to_decimal(Sexagesimal),
		    case Ref of
			["E"] -> Dec;
			["W"] -> -Dec
		    end
	    end
    end.

sexagesimal_to_decimal([{N1,D1}, {N2,D2}, {N3,D3}]) ->
    (N1/D1) + ((N2/D2) / 60) + ((N3/D3) / 3600);
sexagesimal_to_decimal(_) ->
    undefined.
