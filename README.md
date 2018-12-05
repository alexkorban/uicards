# UICards

UICards are a tool for live UI development. 

Crafting a UI can suck up a lot of time due to its stateful nature. We're all familiar with this cycle: make some tweaks, reload the app, navigate to the relevant page, click around and type for a while to get the right state, only to see that the spacing in one of the dropdowns still doesn't look quite right. Rinse and repeat. 

UICards allow you to lay out pieces of UI on a single page, and to have these pieces *actually work* - responding to clicks, typing and so on. 

Combined with live reloading (eg using `elm-live`), this provides an environment for interactive UI development.

Pieces of UI are organised into multiple cards, and cards can be organised into decks for convenience (mainly to avoid too much scrolling).

Cards are defined in an Elm file which looks something like this:

```elm
module Cards exposing (main)

import UICards exposing (card, cardError, deck, show)
import Main as App -- This is your application's main module

initialMenuModel =
    { page = MenuPage { isMenuOpen = False, ... } }

initialCardModel =
    { page = CardPage { cardTitle = "Test card", ... } }

main =
    show App.update
        [ deck "Menu elements"
            [ card "Menu button" initialMenuModel <|
                \model ->
                    case model.page of
                        MenuP page ->
                            App.menuButton page

                        _ ->
                            cardError "Invalid page in the model"

            , card "Menu panel" initialMenuModel <|
                \_ ->
                    App.menuPanel 
                        [ { link = "#settings", name = "Settings" }
                        , { link = "#logout", name = "Logout" } 
                        ]
            ]

        , deck "Card elements"
            [ card "Card" initialCardModel <|
                \model ->
                    case model.page of
                        CardP page ->
                            App.cardHtml page.cardTitle 

                        _ ->
                            cardError "Invalid page in the model"

            , card "Error test" initialCardModel <|
                \_ ->
                    cardError "This is a test"
            ]
        ]
```

The cards file can simply be compiled with `elm make` to produce an HTML or JS file. 

For live interactive development, `elm-live` is the easiest option:

```
$ elm-live src/Cards.elm 
```

The `show` function is given the `update` function from your application as its argument, and each card gets an initial model which allows you to render UI elements in the states you're interested in. 