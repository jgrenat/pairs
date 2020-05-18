module Animal exposing (Emoji, emojis, toString)


type Emoji
    = Emoji Int


toString : Emoji -> String
toString (Emoji code) =
    code
        |> Char.fromCode
        |> String.fromChar


emojis : List Emoji
emojis =
    List.map Emoji
        [ 0x0001F400
        , 0x0001F403
        , 0x0001F404
        , 0x0001F405
        , 0x0001F406
        , 0x0001F407
        , 0x0001F408
        , 0x0001F409
        , 0x0001F40A
        , 0x0001F40C
        , 0x0001F40D
        , 0x0001F40E
        , 0x0001F40F
        , 0x0001F410
        , 0x0001F411
        , 0x0001F412
        , 0x0001F413
        , 0x0001F414
        , 0x0001F415
        , 0x0001F416
        , 0x0001F417
        , 0x0001F418
        , 0x0001F419
        , 0x0001F41B
        , 0x0001F41C
        , 0x0001F41D
        , 0x0001F41E
        , 0x0001F420
        , 0x0001F421
        , 0x0001F422
        , 0x0001F423
        , 0x0001F426
        , 0x0001F427
        , 0x0001F428
        , 0x0001F429
        , 0x0001F42A
        , 0x0001F42B
        , 0x0001F42C
        , 0x0001F42D
        , 0x0001F433
        , 0x0001F438
        , 0x0001F439
        , 0x0001F43A
        , 0x0001F43B
        , 0x0001F43C
        , 0x0001F43F
        , 0x0001F54A
        , 0x0001F577
        , 0x0001F980
        , 0x0001F981
        , 0x0001F982
        , 0x0001F983
        , 0x0001F984
        , 0x0001F985
        , 0x0001F986
        , 0x0001F987
        , 0x0001F988
        , 0x0001F989
        , 0x0001F98A
        , 0x0001F98B
        , 0x0001F98C
        , 0x0001F98D
        , 0x0001F98E
        , 0x0001F98F
        , 0x0001F990
        , 0x0001F991
        , 0x0001F992
        , 0x0001F993
        , 0x0001F994
        , 0x0001F995
        , 0x0001F997
        , 0x0001F998
        , 0x0001F999
        , 0x0001F99A
        , 0x0001F99B
        , 0x0001F99C
        , 0x0001F99D
        , 0x0001F99E
        , 0x0001F99F
        , 0x0001F9A1
        , 0x0001F9A2
        , 0x0001F9A5
        , 0x0001F9A6
        , 0x0001F9A7
        , 0x0001F9A8
        , 0x0001F9A9
        ]
