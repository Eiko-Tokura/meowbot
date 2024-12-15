This is a cute qq bot 'MeowMeow' I wrote in haskell owo

It was my first Haskell project and was not well written. But I have now revisited it and heavily refactored, it is now much more modular, maintainable! I also added a lot exciting new features!

## Features
- [x] Chat with you via ChatGPT API
- [x] Send you interesting voices (aokana >w<)
- [x] Generating really good pictures, formulas from Markdown
- [x] Generating Chat output in Markdown and display in pictures
- [x] Generating Random Variables
- [x] Polling
- [x] User, group and command management
- [x] Automatic retract user messages in group
- [x] Proxy WebSocket connection to connect to external servers
- [x] Run as client mode or server mode, or many of them at once
- [x] Full async and concurrent support

When using the bot, you can use ':help' to get more information about each command.

Welcome to contribute and play with MeowMeow!

## Usage

If you want to run it, you need the following files to support the above functions: (otherwise the corresponding functions will not work)

1. LLOneBot framework (replacing cqhttp) or natpcat.
2. Create a file 'apiKey' containing the API key of ChatGPT in the root directory of the project. Otherwise, the ChatGPT function will not work.
3. Add aokana voice files in ./aokana/voice and script files in ./aokana/scripts. Otherwise, the aokana function will not work.

Compile and run it in either ghc, cabal or stack, whatever >w<

* Using cabal

  We are using the newest and coolest `ghc-9.10.1` compiler. If you haven't installed `ghc`, `cabal`, nor `stack`, install `ghcup` first is recommended, find it [here](https://www.haskell.org/ghcup/).

  ```bash
  git clone https://Eiko-Tokura/meowbot
  cd meowbot
  cabal build
  ```

* Using stack

  Install stack via `curl -sSL https://get.haskellstack.org/ | sh` or via your package manager, also find their [website](https://docs.haskellstack.org/en/stable/install_and_upgrade/). Then run the following commands:

  ```bash
  git clone https://Eiko-Tokura/meowbot
  cd meowbot
  stack init
  stack build
  ```

### Command Line Arguments

```
Usage: MeowBot [--run-client <ip> <port> | --run-server <ip> <port>] [--name <name>] [--sys-msg <msg>] [--command <commandId>] [--debug-json] [--debug-cqmsg] [--proxy <address> <port>]
  --run-client <ip> <port>  : run the bot as a client connecting to the go-cqhttp WebSocket server
  --run-server <ip> <port>  : run the bot as a server, using reverse WebSocket connection
  --name <name>             : set the name of the bot
  --sys-msg <msg>           : set the global system message of the bot
  --command <commandId>     : allow the bot to use the command with the given commandId, use multiple --command flags to allow multiple commands
                              commandId must be one of [Aokana,Cat,Help,Md,Random,Retract,System,User,Study,BookMan,Poll]
                              if no --command flags are given, the bot will use all commands
  --debug-json              : print the JSON message received from the server
  --debug-cqmsg             : print the decoded CQMessage
  --proxy <address> <port>  : set the proxy server to connect to, use multiple --proxy flags to connect to multiple servers
  If no arguments are given, the bot will run as a client connecting to the go-cqhttp WebSocket server on 127.0.0.1:3001

Multiple bots can be started by using multiple sets of flags, starting with a run flag followed by other flags.
```

### To Do

- [ ] Adding TUI interface and control panel
- [ ] ChatGPT Function Calling
- [ ] Assistant And Notes Function
- [x] Using database to store user data and statistics
- [ ] Provide interesting statistics functions to users
- [ ] Bring back the funny hangman game
- [x] Implement common data shared among concurrent bot instances
- [x] Proper logging system
- [x] Modular system using type level programming and monad transformers
- [ ] Upload `item` as a separate package to hackage
- [ ] Refactor commands like `aokana` and `study` to use the database
