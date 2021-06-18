module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Minigame =
    { img: string
      title: string
      description: string
      gh: string }

type Model = { minigames: Minigame list }

type Msg = Msg of string

let init () : Model * Cmd<Msg> =
    let model =
        { minigames =
              [
                  {
                    img = "/bridge-game.png"
                    title = "Bridge Fight"
                    description = "A game where you use limited blocks to pass your opponent and score in their goal."
                    gh = "bridgefight"}
                  {
                    img = "/wool-world.png"
                    title = "Wool World"
                    description = "A world of infinite wool and creativity."
                    gh = "Wool-World"}
                  {
                    img = "/ice-boom.png"
                    title = "Ice Boom"
                    description = "Explode opponents off an ever shrinking platform."
                    gh = "iceboom"}
                  {
                    img = "/survival.png"
                    title = "Survival"
                    description = "Vanilla minecraft, adapted for the PRIMD server."
                    gh = "HubVanilla"}
                  {
                    img = "/missile-mayhem.png"
                    title = "Missile Mayhem"
                    description = "A missile wars remake where you send missiles at your opponent to win."
                    gh = "MissileMayhem"}
              ]
        }

    let cmd = Cmd.none

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> = model, Cmd.none

open Feliz
open Feliz.Bulma

let ruleText (rule: string) =
    Bulma.box [
        Bulma.color.hasBackgroundBlackTer
        Bulma.color.hasTextGreyLighter
        prop.text rule
    ]

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
let minigames model dispatch =
    Bulma.tile [
        Bulma.spacing.mx6
        prop.children [
            for minigame in model.minigames ->
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
                                Bulma.image.is4by3
                                prop.children [
                                    Html.img [ prop.src minigame.img ]
                                ]
                            ]
                        ]
                        Bulma.cardContent [
                            Bulma.title [
                                Bulma.color.hasTextGreyLighter
                                prop.text minigame.title
                            ]
                            Bulma.content minigame.description
                            ghLink minigame.gh
                        ]
                    ]
                ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
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
                ]
                prop.children [
                    Bulma.heroBody [
                        prop.style [ style.alignSelf.center ]
                        prop.children [
                            Html.img [ prop.src "/primd_logo.png" ]
                        ]
                    ]
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
            Bulma.title [
                Bulma.color.hasTextGreyLighter
                Bulma.spacing.mt6
                Bulma.text.hasTextCentered
                Bulma.title.is2
                prop.text "Server Rules"
            ]
            Bulma.content [
                Bulma.color.hasTextGreyLighter
                prop.style [
                    style.marginLeft (length.rem 13)
                    style.marginRight (length.rem 13)
                ]
                Bulma.title.is2
                prop.children [
                    ruleText "I. Treat all players with respect. No derogatory, discriminatory, or hateful speech will be tolerated. This includes trolling,  harassment, doxxing, and scamming others."
                    Html.br []
                    ruleText "II. Have an appropriate username and skin."
                    Html.br []
                    ruleText "III. Don't cheat. Hacks, cross-teaming, team griefing, lag machines, alt. account abuse, spawn trapping  & auto-click/macros are considered cheating. Please report bugs to moderators."
                    Html.br []
                    ruleText "IV. No advertising of social media posts, videos, accounts, or channels without express permission from a PRIMD server moderator."
                    Html.br []
                    ruleText "V. Primd reserves the right to amend this document and expel or ban players who do not follow these bylaws without notice."
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
