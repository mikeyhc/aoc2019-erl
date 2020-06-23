-module(space_image).

-export([read_image/3, read_parts/1, checksum/1, show_image/3]).

read_image(Path, Width, Height) ->
    ImageSize = Width * Height,
    Parts = read_parts(Path),
    build_layers(ImageSize, Parts, []).

read_parts(Path) ->
    {ok, Bin} = file:read_file(Path),
    String = binary:bin_to_list(Bin),
    List = string:trim(String),
    lists:map(fun(X) -> X - $0 end, List).

build_layers(_Size, [], Output) ->
    lists:reverse(Output);
build_layers(Size, Input, Output) ->
    {Layer, Rest} = lists:split(Size, Input),
    build_layers(Size, Rest, [Layer|Output]).

count_elements(Element, {Zeros, Ones, Twos}) ->
    case Element of
        0 -> {Zeros + 1, Ones, Twos};
        1 -> {Zeros, Ones + 1, Twos};
        2 -> {Zeros, Ones, Twos + 1};
        _ -> throw({invalid_element, Element})
    end.

build_checksum(Image) ->
    {Zeros, Ones, Twos} = lists:foldl(fun count_elements/2, {0, 0, 0}, Image),
    {Zeros, Ones * Twos}.

checksum(Image) ->
    Checksums = lists:map(fun build_checksum/1, Image),
    [{_, Checksum}|_] = lists:keysort(1, Checksums),
    Checksum.

process_pixel(Pixel, {Image, Idx}) ->
    case Pixel of
        0 -> {array:set(Idx, 0, Image), Idx+1};
        1 -> {array:set(Idx, 1, Image), Idx+1};
        2 -> {Image, Idx+1};
        _ -> throw({invalid_element, Pixel})
    end.

build_image(Layer, Image) ->
    {I0, _} = lists:foldl(fun process_pixel/2, {Image, 0}, Layer),
    I0.

pixels_to_string(Pixels) ->
    F = fun(0) -> $ ;
           (1) -> $#
        end,
    lists:map(F, Pixels).

render_image(Image, Width) ->
    Rows = build_layers(Width, Image, []),
    Values = lists:map(fun pixels_to_string/1, Rows),
    lists:foreach(fun(X) -> io:format("~s~n", [X]) end, Values).

show_image(Image, Height, Width) ->
    ImageSize = Height * Width,
    ActualImage0 = array:new(ImageSize),
    ActualImage1 = lists:foldl(fun build_image/2, ActualImage0,
                               lists:reverse(Image)),
    render_image(array:to_list(ActualImage1), Height).
