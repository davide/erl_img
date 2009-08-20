%%% File    : img_tif.erl
%%% Author  : Tony Rogvall <tony@a55.hemma.se>
%%% Description : TIFF image format
%%% Created :  6 Mar 2003 by Tony Rogvall <tony@a55.hemma.se>

-module(image_tiff).

-include("erl_img.hrl").
-include("tiff.hrl").

-include("api.hrl").
-include("dbg.hrl").

-export([scan_fd/3, scan_file/3, scan_binary/3]).
-export([dump_file/1, dump_binary/1]).

-import(tiff, [scan_ifd/6, scan_ifd_bin/6, undo_differencing/4, unpack_bits/1, decode_tag/1]).
-import(lists, [map/2, reverse/1]).

-define(II, 16#4949).  %% little-endian
-define(MM, 16#4D4D).  %% big-endian
-define(MAGIC, 42).

magic(<<?II:16,?MAGIC:16/little,_Offset:32/little,_/binary>>) -> true;
magic(<<?MM:16,?MAGIC:16/big,_Offset:32/big,_/binary>>) -> true;
magic(_) -> false.

mime_type() -> "image/tiff".
    
extensions() -> [".tiff", ".tif" ].


read_info(Fd) ->
    case scan_fd(Fd, fun collect_fun/3, #erl_image { type = ?MODULE }) of
	{ok, IMG} ->
	    Bps = erl_img:attribute(IMG, 'BitsPerSample'),
	    Xs  = erl_img:attribute(IMG,'ExtraSamples',[]),
	    Format = 
		case erl_img:attribute(IMG, 'PhotoMetricInterpretation') of
		    [0] ->
			case Bps of
			    [4] -> gray4;
			    [8] -> gray8
			end;
		    [1] ->
			case Bps of
			    [4] -> gray4;
			    [8] -> gray8
			end;
		    [2] ->
			case Bps of
			    [8,8,8] ->
				case Xs of
				    []  ->  r8g8b8;
				    [_] -> r8g8b8a8
				end;
			    [8,8,8,8] ->
				r8g8b8a8
			end;
		    [3] ->
			case Bps of
			    [4] -> palette4;
			    [8] -> palette8
			end
		end,
	    {ok, IMG#erl_image { format = Format }};
	Error -> 
	    Error
    end.
		    

write_info(_Fd, _IMG) ->
    ok.

read(Fd, IMG) ->
    read(Fd, IMG, 
	 fun(_, Row, Ri, St) ->
		 ?dbg("tiff: load row ~p\n", [Ri]),
		 [{Ri,Row}|St] end, 
	 []).


read(Fd,IMG, RowFun, St0) ->
    SOffset       = erl_img:attribute(IMG, 'StripOffset'),
    SCount        = erl_img:attribute(IMG, 'StripByteCounts'),
    [Compression] = erl_img:attribute(IMG, 'Compression',[1]),
    [Predict]     = erl_img:attribute(IMG, 'Predictor', [0]),
    [FillOrder]   = erl_img:attribute(IMG, 'FillOrder', [1]),
    {SampleOrder,Y0,Ys} = case erl_img:attribute(IMG, 'Orientation', [1]) of
			      [1] -> {left_to_right, 0, 1};
			      [2] -> {right_to_left, 0, 1};
			      [3] -> {right_to_left, IMG#erl_image.height-1,-1};
			      [4] -> {left_to_right, IMG#erl_image.height-1,-1}
			end,
    BytesPerRow = (IMG#erl_image.depth div 8) * IMG#erl_image.width,
    ?dbg("BytesPerRow = ~p\n", [BytesPerRow]),
    IMG1 = IMG#erl_image { order = SampleOrder },
    PIX = #erl_pixmap { width = IMG1#erl_image.width,
			height = IMG1#erl_image.height,
			format = IMG1#erl_image.format },
    case read_strips(Fd,PIX,RowFun,St0,Y0,Ys,BytesPerRow,
		     Compression, Predict, FillOrder,SOffset,SCount) of
	{ok, PIX1} ->
	    {ok, IMG1#erl_image { pixmaps = [PIX1]}};
	Error ->
	    Error
    end.


read_strips(_Fd,PIX,_RowFun,St0,_Ri,_Rs,
	    _BytesPerRow,_Compression, _Predict, _Fill, [], []) ->
    {ok, PIX#erl_pixmap { pixels = St0 }};
read_strips(Fd,PIX,RowFun,St0,Ri,Rs,
	    BytesPerRow,Compression, Predict, Fill,
	    [Offs|SOffset], [Size|SCount]) ->
    case file:pread(Fd,Offs,Size) of
	{ok,Bin} ->
	    case Compression of
		1 -> %% no compression
		    {St1,Rj} = split_strip(PIX,Bin,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression,Predict,Fill,SOffset,SCount);

		5 -> %% lzw compression
		    Bin1 = lzw:decompress_tiff(Bin, 8, Fill),
		    Bin2 = undo_differencing(Bin1, Predict,
					     PIX#erl_pixmap.format,
					     PIX#erl_pixmap.width),
		    {St1,Rj} = split_strip(PIX,Bin2,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression, Predict, Fill, SOffset, SCount);
		
		32773 ->
		    Bin1 = unpack_bits(Bin),
		    {St1,Rj} = split_strip(PIX,Bin1,BytesPerRow,
					   RowFun,St0,Ri,Rs),
		    read_strips(Fd,PIX,RowFun,St1,Rj,Rs,BytesPerRow,
				Compression, Predict, Fill, SOffset, SCount);
		_ ->
		    {error, {unknown_compression,Compression}}
	    end;
	Error ->
	    Error
    end.


split_strip(PIX,Strip,RowWidth,RowFun,St0,Ri,Rs) ->
    case Strip of
	<<Row:RowWidth/binary, Tail/binary>> ->
	    St1 = RowFun(PIX,Row,Ri,St0),
	    split_strip(PIX,Tail,RowWidth,RowFun,St1,Ri+Rs,Rs);
	_ ->
	    {St0,Ri}
    end.


write(_Fd,_IMG) ->
    ok.

%% Image info collector functions
collect_fun(_Fd, T, St) ->
    Key = tiff:decode_tag(T#tiff_entry.tag),
    Value = T#tiff_entry.value,
    As = [{Key,Value} | St#erl_image.attributes],
    case Key of
	'ImageWidth' ->
	    [Width] = Value,
	    St#erl_image { width = Width, attributes = As };
	'ImageLength' ->
	    [Length] = Value,
	    St#erl_image { height = Length, attributes = As };
	'BitsPerSample' ->
	    St#erl_image { depth = lists:sum(Value), attributes = As };
	'ImageDescription' ->
	    St#erl_image { comment = Value, attributes = As };
	'DateTime' ->
	    [V] = Value,
	    case string:tokens(V, ": ") of
		[YYYY,MM,DD,H,M,S] ->
		    DateTime = {{list_to_integer(YYYY),
				 list_to_integer(MM),
				 list_to_integer(DD)},
				{list_to_integer(H),
				 list_to_integer(M),
				 list_to_integer(S)}},
		    St#erl_image { itime = DateTime, attributes = As };
		_ ->
		    St#erl_image { attributes = As }
	    end;

	_ ->
	    St#erl_image { attributes = As }
    end.


dump_fun(_Fd, T, St) ->
    Key = tiff:decode_tag(T#tiff_entry.tag),
    io:format("~s ~s ~w\n", [Key,T#tiff_entry.type,T#tiff_entry.value]),
    St.

dump_binary(Bin) when is_binary(Bin) ->
    scan_binary(Bin, fun dump_fun/3, ok).

dump_file(File) ->
    scan_file(File, fun dump_fun/3, ok).


scan_file(File, Callback, St) ->
    case file:open(File, [raw, binary, read]) of
	{ok,Fd} ->
	    Res = scan_fd(Fd, Callback, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.	

scan_binary(Bin, Callback, St) ->
    case file:open(Bin, [ram, binary, read]) of
	{ok,Fd} ->
	    Res = scan_fd(Fd, Callback, St),
	    file:close(Fd),
	    Res;
	Error ->
	    Error
    end.


scan_fd(Fd, Callback, St) ->
    case file:read(Fd, 8) of
	{ok, <<?II:16,?MAGIC:16/little,Offset:32/little>>} ->
	    %% io:format("TIFF: LITTLE endian\n"),
	    scan_ifd(Fd, [$0], Offset, little, Callback, St);
	{ok, <<?MM:16,?MAGIC:16/big,Offset:32/big>>} ->
	    %% io:format("TIFF: BIG endian\n"),
	    scan_ifd(Fd, [$0], Offset, big, Callback, St);
	{ok,_} ->
	    {error, bad_magic};
	Error -> 
	    Error
    end.

