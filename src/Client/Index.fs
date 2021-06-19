module Index

open Elmish

type Minigame =
    { img: string
      title: string
      description: string
      md: string
      gh: string }

type Model = { minigames: Minigame list; scroll: double; baseState: bool }

type Msg = Scroll of double

let init () : Model * Cmd<Msg> =
    let model =
        { minigames =
              [
                  {
                    img = "/bridge-game.png"
                    title = "Bridge Fight"
                    description = "A game where you use limited blocks to pass your opponent and score in their goal."
                    md = "/bridge-game.md"
                    gh = "bridgefight"}
                  {
                    img = "/wool-world.png"
                    title = "Wool World"
                    description = "A world of infinite wool and creativity."
                    md = "/wool-world.md"
                    gh = "Wool-World"}
                  {
                    img = "/ice-boom.png"
                    title = "Ice Boom"
                    description = "Explode opponents off an ever shrinking platform."
                    md = "/ice-boom.md"
                    gh = "iceboom"}
                  {
                    img = "/survival.png"
                    title = "Survival"
                    description = "Vanilla minecraft, adapted for the PRIMD server."
                    md = "/survival.md"
                    gh = "HubVanilla"}
                  {
                    img = "/missile-mayhem.png"
                    title = "Missile Mayhem"
                    description = "A missile wars remake where you send missiles at your opponent to win."
                    md = "/missile-mayhem.md"
                    gh = "MissileMayhem"}
              ]
          scroll = 0.0
          baseState = true
        }

    let cmd = Cmd.none

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
        | Scroll s ->
            printfn "%f" s
            {model with scroll=s;baseState=false}, Cmd.none

open Feliz
open Feliz.Bulma

[<ReactComponent>]
let iconText model =
    Bulma.icon [
        prop.style [
            style.alignSelf.center
            style.bottom (length.rem 1)
            style.position.absolute
            style.opacity (1.-model.scroll/100.)
        ]
        prop.children [
            Html.i [
                prop.className "fas fa-arrow-down"
            ]
        ]
    ]

[<ReactComponent>]
let ruleText (rule: string) =
    Bulma.box [
        Bulma.color.hasBackgroundBlackTer
        Bulma.color.hasTextGreyLighter
        prop.text rule
    ]

[<ReactComponent>]
let ghLink (name:string) =
    Html.a [
        prop.children [
            Bulma.panelIcon [
                Html.img [
                    prop.src "/GitHub-Mark-Light-64px.png"
                    prop.style [
                        style.marginTop (length.rem 0.5)
                    ]
                ]
            ]
            Html.span name
        ]
        prop.href ("https://github.com/primd-os/"+name)
    ]

[<ReactComponent>]
let minigames model dispatch =
    let pad cols games =
        {img="/coming_soon.png";title="Coming Soon";description="More games are always being developed, and anyone can contribute games if they want.";gh="Hub";md="coming_soon.md"}
        |> List.replicate (cols-(List.length games))
        |> List.append games
    let cols = 3
    Bulma.tile [
        prop.style [
            style.marginLeft (length.rem 13)
            style.marginRight (length.rem 13)
        ]
        Bulma.tile.isVertical
        prop.children [
            for minigameGroup in model.minigames |> List.chunkBySize cols ->
                Bulma.tile [
                    Bulma.spacing.my2
                    prop.children [
                        for minigame in minigameGroup |> pad cols ->
                            let modalState, toggleState = React.useState(false)
                            Bulma.card [
                                Bulma.spacing.mx2
                                Bulma.color.hasBackgroundBlackTer
                                Bulma.color.hasTextGreyLighter
                                prop.style [
                                    style.borderRadius 10
                                    style.overflow.hidden
                                    style.width (length.percent 100)
                                ]
                                prop.children [
                                    Bulma.cardImage [
                                        Bulma.image [
                                            prop.onClick (fun _ -> toggleState(true))
                                            Bulma.image.is4by3
                                            prop.children [
                                                Html.img [ prop.src minigame.img ]
                                            ]
                                        ]
                                    ]
                                    Bulma.cardContent [
                                        prop.onClick (fun _ -> toggleState(true))
                                        prop.children [
                                            Bulma.title [
                                                Bulma.color.hasTextGreyLighter
                                                prop.text minigame.title
                                            ]
                                            Bulma.content minigame.description
                                            ghLink minigame.gh
                                        ]
                                    ]
                                    Bulma.modal [
                                        prop.onClick (fun _ -> toggleState(false))
                                        if modalState then Bulma.modal.isActive
                                        prop.children [
                                            Bulma.modalBackground []
                                            Bulma.modalContent [
                                                Bulma.box [
                                                    Bulma.color.hasBackgroundBlackTer
                                                    prop.children [
                                                        Bulma.title [
                                                            Bulma.color.hasTextGreyLighter
                                                            prop.text minigame.title
                                                        ]
                                                        Bulma.content [
                                                            Bulma.color.hasTextGreyLighter
                                                            prop.text minigame.description
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                    ]
                ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    if model.baseState then Browser.Dom.window.addEventListener ("scroll", (fun _ -> dispatch (Scroll Browser.Dom.window.scrollY)))
    Bulma.block [
        Bulma.color.hasBackgroundBlackBis
        prop.children [
            Bulma.hero [
                hero.isFullHeight
                color.isPrimary
                prop.style [
                    style.backgroundSize "cover"
                    style.backgroundImageUrl "/background.png"
                    style.backgroundPosition "no-repeat center center fixed"
                    style.position.relative
                ]
                prop.children [
                    Bulma.heroBody [
                        prop.style [ style.alignSelf.center ]
                        prop.children [
                            Html.img [ prop.src "/primd_logo.png" ]
                        ]
                    ]
                    iconText model
                ]
            ]
            Bulma.title [
                Bulma.color.hasTextGreyLighter
                Bulma.spacing.mt6
                Bulma.text.hasTextCentered
                Bulma.title.is1
                prop.text "PRIMD Minecraft Server"
            ]
            Bulma.subtitle [
                Bulma.text.hasTextCentered
                prop.style [
                    style.lineHeight 30
                ]
                prop.children [
                    Html.code [
                        Bulma.color.hasBackgroundBlackTer
                        prop.text "play.primd.net"
                    ]
                    Html.br []
                    ghLink "Hub"
                ]
            ]
            minigames model dispatch
            Bulma.columns [
                columns.isGapless
                prop.style [
                    style.marginTop (length.rem 2)
                    style.marginLeft (length.rem 13)
                    style.marginRight (length.rem 13)
                ]
                prop.children [
                    Bulma.column [
                        Bulma.title [
                            Bulma.color.hasTextGreyLighter
                            Bulma.text.hasTextCentered
                            Bulma.title.is2
                            prop.style [
                                style.marginTop (length.percent 40)
                            ]
                            prop.text "Server Rules"
                        ]
                    ]
                    Bulma.column [
                        column.isThreeFifths
                        prop.children [
                            Bulma.content [
                                Bulma.color.hasTextGreyLighter
                                Bulma.title.is2
                                prop.children [
                                    ruleText "I. Treat all players with respect. No derogatory, discriminatory, or hateful speech will be tolerated. This includes trolling,  harassment, doxxing, and scamming others."
                                    ruleText "II. Have an appropriate username and skin."
                                    ruleText "III. Don't cheat. Hacks, cross-teaming, team griefing, lag machines, alt. account abuse, spawn trapping  & auto-click/macros are considered cheating. Please report bugs to moderators."
                                    ruleText "IV. No advertising of social media posts, videos, accounts, or channels without express permission from a PRIMD server moderator."
                                    ruleText "V. Primd reserves the right to amend this document and expel or ban players who do not follow these bylaws without notice."
                                ]
                            ]
                        ]
                    ]
                ]
            ]
            Bulma.level [
                Bulma.size.isSize5
                Bulma.spacing.mt6
                Bulma.spacing.pb3
                prop.children [
                    Bulma.levelItem [
                        Html.a [
                            Bulma.text.hasTextCentered
                            prop.children [
                                Html.img [
                                    Bulma.spacing.mx3
                                    prop.src "/Discord-Logo-Color.png"
                                    prop.style [
                                        style.height 32
                                        style.maxHeight 32
                                    ]
                                ]
                            ]
                            prop.href "http://discord.gg/S6TUQWt"
                        ]
                        Html.a [
                            Bulma.text.hasTextCentered
                            prop.children [
                                Html.img [
                                    Bulma.spacing.mx3
                                    prop.src "/GitHub-Mark-Light-64px.png"
                                    prop.style [
                                        style.height 32
                                        style.maxHeight 32
                                    ]
                                ]
                            ]
                            prop.href "https://github.com/primd-os"
                        ]
                    ]
                ]
            ]
        ]
    ]
