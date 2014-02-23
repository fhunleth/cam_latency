-module(frame_decoder).

-export([decode/1]).

%% Parse the multipart headers and assemble a list of frames
-spec decode(binary()) -> {binary(), [binary()]}.
decode(Buffer) ->
    {NewBuffer, FramesInReverse} = decode(Buffer, []),
    {NewBuffer, lists:reverse(FramesInReverse)}.

decode(Buffer, PreviousFrames) ->
    Boundary = <<"foofoo">>,
    case cow_multipart:parse_headers(Buffer, Boundary) of
	{ok, _Headers, Rest} ->
	    case cow_multipart:parse_body(Rest, Boundary) of
		{done, Frame, NextPart} ->
		    decode(NextPart, [Frame|PreviousFrames]);
		_ ->
		    {Buffer, PreviousFrames}
	    end;
	_ ->
	    {Buffer, PreviousFrames}
    end.
