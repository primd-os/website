module Index

open Elmish
open System.Text.RegularExpressions

let (|CompiledMatch|_|) pattern input =
    if isNull input then
        None
    else
        let m =
            Regex.Match(input, pattern, RegexOptions.Singleline)

        if m.Success then
            Some [ for x in m.Groups -> x ]
        else
            None

type Minigame =
    { Img: string
      Title: string
      Description: string
      Md: string
      Gh: string }

type Model = { Minigames: Minigame list; Scroll: double; BaseState: bool }

type Msg = Scroll of double

let init () : Model * Cmd<Msg> =
    let model =
        { Minigames =
              [
                  {
                    Img = "/bridge-game.png"
                    Title = "Bridge Fight"
                    Description = "A game where you use limited blocks to pass your opponent and score in their goal."
                    Md =
                        """
# Bridge Fight
## Goal
To jump into the hoop below your opponent's spawn 5 times
## Scaffolding
Each round you are given a block which you can use to make barriers for your opponent or get around your opponent's barriers.
After placing this block you are given a second block which can only be placed against the first block and will destroy the first block.
## Items (per round)
* Iron Sword
  * Can destroy all placed blocks but not original map blocks
* One scaffolding block
* One Bow
* 8 Golden Apples
* 16 Arrows
* Leather armor
                        """
                    Gh = "bridgefight"}
                  {
                    Img = "/missile-mayhem.png"
                    Title = "Missile Mayhem"
                    Description = "A missile wars remake where you send missiles at your opponent to win."
                    Md =
                        """
# Missile Mayhem
## Items
* Bow / Arrows
  * Bow has knockback and flame
  * Given 2 arrows per collection and on start and stack infinitely
* Tomohawk
  * A basic missile
  * Given on start
* Shieldbuster
  * A missile that is able to fly through blocks without stopping
* Lightning
  * A missile that flies at twice the speed
* Juggernaut
  * A missile with extra tnt
* Custom Missile
  * A 3x3x17 size missile you can build in the lobby
  * Can have upto 12 tnt
* Shield
  * A snowball that when thrown turns into a glass shield that can stop missiles
  * Given on start
* Fireball
  * Can be placed down and punched or put in offhand to load it into bow
  * A projectile that explodes on impact
  * Given on start
* TNT
  * Only availible by breaking it and is the only block you can collect and place
## Missile Sets
* New - Hard to ride, but good at what they do
* Old - Slightly slower missiles, but have a large amonunt of easily defusable tnt
* Thin - 1 wide missiles, more easy to be knocked off of but more resistant to being blown up
* Flat - 1 tall missiles, hard to be knoked off of but morer difficult to defuse
## Team types
* 2 teams
* 4 teams
## Game modes
* Normal
  * Destroy the wall behind your opponent's base to win
* King of the Hill
  * Stand on the center platform for 2.5 minutes to win
* Capture the Flag
  * Capture the flag behind your opponent's base and bring it back to your own to win
## Maps
* Normal, No Walls, Small Walls, Center Wall, Platforms, Weak Center, Bridge, and Diagonal
* Game is a random map from the players in the game
## Commands - activited via /trigger
* ToggleRanked - Toggles between doing ranked matches and not. Games will only be ranked if every player has ranked on.
* JoinSpectators - Allows you to switch to spectator team mid game
* JoinGreen - Can be used to switch team to green in queue
* JoinBlue - Can be used to switch team to blue in queue
* JoinRed - Can be used to switch team to red in queue
* JoinBlack - Can be used to switch team to black in queue
* Practice - Moves you to practice where you have infinite items
* ClearPractice - Clears the practice area you're in
* Heal - Can be used in practice to instantly heal to full health
* Leave - Quit to lobby
* ChooseMap - Get a menu to switch your map
* MapVote - Select a map id to vote for
* GiveBook - Get a book explaining the game
* ChooseMissileSet - Select a missile set id
* MissileSetSelect - Get a menu to switch your missile set
                        """
                    Gh = "MissileMayhem"}
                  {
                    Img = "/ice-boom.png"
                    Title = "Ice Boom"
                    Description = "Explode opponents off ever shrinking platforms."
                    Md =
                        """
# Ice Boom
## Goal
To remain on the top ice platform while knocking your opponents off.
## Mechanics
You play on a platform of string where:
* Air turns to frosted ice
* Water turns to string
This means you can either break the string with your fist or blow it up with the creeper eggs you are given. Note you do not take damage so dying directly to the explosion is not a concern.
You can use either punching or explosions to knock your opponents off.
## Maps
* Small
  * Everyone spawns in the same place
  * Given 16 creeper eggs
* Big
  * Everyone spawns in different places with a ice platform below them
  * Given 32 creeper eggs
  * Map size is twice as large
                        """
                    Gh = "iceboom"}
                  {
                    Img = "/wool-world.png"
                    Title = "Wool World"
                    Description = "A world of infinite wool and creativity."
                    Md =
                        """
# Wool World
## Items
* Infinite wool of Blue, Red, White, Black, Green, Purple, magenta
* Infinite signs
* Efficency 1 Shears
## Commands
* Spawnpoint - set your spawnpoint at your current location
                        """
                    Gh = "Wool-World"}
                  {
                    Img = "/survival.png"
                    Title = "Survival"
                    Description = "Vanilla minecraft, adapted for the PRIMD server."
                    Md =
                        """
# Survival
Pure vanilla survival experience
                        """
                    Gh = "HubVanilla"}
              ]
          Scroll = 0.0
          BaseState = true
        }

    let cmd = Cmd.none

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
        | Scroll s ->
            {model with Scroll=s;BaseState=false}, Cmd.none

open Feliz
open Feliz.Bulma

let rec mdList (listMd: string) =
    match listMd with
    | CompiledMatch @"^\n(.*)" [_;rest] ->
        mdList rest.Value
    | CompiledMatch @"^\* (([^\n]*?)\n)((?:( +)\* .*?\n)(?:\4 *\* .*?\n)*)(.*)" [_;_;firstline;subelems;_;rest] ->
        List.Cons (Html.li [
                Bulma.color.hasTextGreyLighter
                prop.children [
                    Html.p [
                        prop.style [
                            style.marginBottom 0
                        ]
                        prop.text firstline.Value
                    ]
                    Html.ul [
                        prop.style [
                            style.marginTop 0
                        ]
                        prop.children (
                            subelems.Value.Split "\n"
                            |> List.ofArray
                            |> List.filter (fun s -> s.Length > 0)
                            |> List.map (fun l ->
                                Regex.Match(l, @" +(.*)", RegexOptions.Singleline).Groups.[1].Value)
                            |> List.fold (fun s e -> s+e+"\n") ""
                            |> mdList)
                    ]
                ]
            ], mdList rest.Value)
    | CompiledMatch @"^\* ((.*?)\n)?(.*)" [_;_;firstline;rest] ->
        List.Cons (Html.li [
                Bulma.color.hasTextGreyLighter
                prop.text firstline.Value
            ], mdList rest.Value)
    | CompiledMatch @"^\s*" [_] ->
        []
    | failState -> failwith ("Invalid State: \""+failState+"\"")

let rec mdElement (markdown: string) =
    match markdown with
    | CompiledMatch @"^# ((.*?)\n)?(.*)" [_;_;firstline;rest] ->
        if firstline.Success then
            List.Cons(
                Html.h1 [
                    Bulma.color.hasTextGreyLighter
                    prop.text firstline.Value
                ],
                mdElement rest.Value)
        else
            Html.h1 [
                Bulma.color.hasTextGreyLighter
                prop.text rest.Value
            ] |> List.singleton
    | CompiledMatch @"^## ((.*?)\n)?(.*)" [_;_;firstline;rest] ->
        if firstline.Success then
            List.Cons(
                Html.h2 [
                    Bulma.color.hasTextGreyLighter
                    prop.text firstline.Value
                ],
                mdElement rest.Value)
        else
            Html.h2 [
                Bulma.color.hasTextGreyLighter
                prop.text rest.Value
            ] |> List.singleton
    | CompiledMatch @"^### ((.*?)\n)?(.*)" [_;_;firstline;rest] ->
        if firstline.Success then
            List.Cons(
                Html.h3 [
                    Bulma.color.hasTextGreyLighter
                    prop.text firstline.Value
                ],
                mdElement rest.Value)
        else
            Html.h3 [
                Bulma.color.hasTextGreyLighter
                prop.text rest.Value
            ] |> List.singleton
    | CompiledMatch @"^((?: *\* .*?\n)+)(.*)" [_;list;rest] ->
        List.Cons(
            Html.ul [
                prop.children (mdList list.Value)
            ],
            mdElement rest.Value)
    | CompiledMatch @"^(.*?)\n(.*)" [_;text;rest] ->
        List.Cons(
            Html.p [
                Bulma.color.hasTextGreyLighter
                prop.text text.Value
            ],
            mdElement rest.Value)
    | text ->
        Html.p [
            Bulma.color.hasTextGreyLighter
            prop.text text
        ] |> List.singleton

[<ReactComponent>]
let IconText model =
    Bulma.icon [
        prop.style [
            style.alignSelf.center
            style.bottom (length.rem 1)
            style.position.absolute
            style.opacity (1.-model.Scroll/100.)
        ]
        prop.children [
            Html.i [
                prop.className "fas fa-arrow-down"
            ]
        ]
    ]

[<ReactComponent>]
let RuleText (rule: string) =
    Bulma.box [
        prop.style [
            style.borderRadius 10
        ]
        Bulma.color.hasBackgroundBlackTer
        Bulma.color.hasTextGreyLighter
        prop.text rule
    ]

[<ReactComponent>]
let GHLink (name:string) =
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
let Minigames model dispatch =
    let pad cols games =
        {
            Img="/coming_soon.png"
            Title="Coming Soon"
            Description="More games are always being developed, and anyone can contribute games if they want."
            Gh="Hub"
            Md=
                """
# Coming Soon
## Potential Upcoming Projects
* A noita esque rougelike
* Extremely customizable PvP
## Contributing
You can suggest features, report bugs, or contribute to development by going to the github links at the bottom of each card.
                """}
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
            for minigameGroup in model.Minigames |> List.chunkBySize cols ->
                Bulma.tile [
                    Bulma.spacing.my2
                    prop.style [
                        style.justifyContent.spaceBetween
                        style.columnGap (length.rem 1)
                    ]
                    prop.children [
                        for minigame in minigameGroup |> pad cols ->
                            let modalState, toggleState = React.useState(false)
                            Bulma.card [
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
                                                Html.img [ prop.src minigame.Img ]
                                            ]
                                        ]
                                    ]
                                    Bulma.cardContent [
                                        prop.onClick (fun _ -> toggleState(true))
                                        prop.children [
                                            Bulma.title [
                                                Bulma.color.hasTextGreyLighter
                                                prop.text minigame.Title
                                            ]
                                            Bulma.content minigame.Description
                                            GHLink minigame.Gh
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
                                                        Bulma.content [
                                                            Bulma.color.hasTextGreyLighter
                                                            prop.children (mdElement minigame.Md)
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
    if model.BaseState then Browser.Dom.window.addEventListener ("scroll", (fun _ -> dispatch (Scroll Browser.Dom.window.scrollY)))
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
                    IconText model
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
                    GHLink "Hub"
                ]
            ]
            Minigames model dispatch
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
                                style.marginTop (length.percent 30)
                            ]
                            prop.text "Server Rules"
                        ]
                    ]
                    Bulma.column [
                        column.isTwoThirds
                        prop.children [
                            Bulma.content [
                                Bulma.color.hasTextGreyLighter
                                Bulma.title.is2
                                prop.children [
                                    RuleText "I. Treat all players with respect. No derogatory, discriminatory, or hateful speech will be tolerated. This includes spawn trapping, harassment, doxxing, and scamming others."
                                    RuleText "II. Have an appropriate username and skin."
                                    RuleText "III. Don't cheat. Hacks, cross-teaming, team griefing, lag machines, alt. account abuse & auto-click/macros are considered cheating. Please report bugs to moderators."
                                    RuleText "IV. No advertising of social media posts, videos, accounts, or channels without express permission from a PRIMD server moderator."
                                    RuleText "V. PRIMD reserves the right to amend this document and expel or ban players who do not follow these rules without notice."
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
