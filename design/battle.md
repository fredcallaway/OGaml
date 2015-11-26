

### Battle

The battle module is the master module for the battle mode of the game. The module interfaces with `Zone` through the `enter_battle` command, which takes a `battle` record and a `player` record, returning a `player option`. If the battle is won, the new player is returned with increased experience and any consumed items removed. If the battle is lost, `None` is returned, and the Zone module resets to the state before the battle was begun.

Battle depends on `Player` and `Item` because stats (e.g. health) are determined by the player and abilities (e.g. slash) are determined by the equipped items. Battle also depends on `Io` because it involves player interaction. The main function in battle is a REPL which presents the player with the state of the battle and asks for a decision to use an ability or a consumable item.