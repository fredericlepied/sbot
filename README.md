# SBot the smart ChatDev bot

## Introduction

SBot is designed to be a robotic member of a team of software
developers. SBot can do actions and inform the team on events.

SBot interacts with members of the team on a chat. The idea is to let
SBot do repetitive tasks that need human in the loop but can be
partially automated. Usually these tasks need a human judgement to
start them or a human to finish them. That is to complement tasks that
can be fully automated in CI/CD systems.

Here are examples of such use cases:

1. apply a Github PR as a patch to a package and update it when it is
   updated and remove it when it is merged.
2. reproduce a CI error and let someone from the team debug it when it
   is reproduced.
3. backport changes to stable branches when asked or a specific Github
   tag is set.

SBot is always connected to be able to interact all the time with its
team. To do so when a configuration is changed or when its code is
updated, it performs a hot reload without needing to restart to avoid
losing the connection.

## Getting started

SBot has been designed to run under Linux systems. Contributions are
welcome to run it under other systems.

First, you need to install swi-prolog on the system hosting your
bot. Under Fedora 27, you just have to do `dnf install pl`. For other
Linux systems just follow the instructions here:
http://www.swi-prolog.org/download/stable .

Then copy the `config.pl.ex` into `config.pl` and edit it to suit your
needs.

Once your configuration is ready, you just have to issue `./bot.sh` to
start the bot.

## Modules

SBot has a modular design to allow teams to choose which modules to
activate according to their needs.

A module is usually one Prolog file and a set of command line
tools. This is designed this way to share tools beween the bot and the
team.

### github module

TBC

### gerrit module

TBC

### fun module

TBC

### self module

TBC

### admin module

TBC

### autoupdate module

TBC

### fedora module

TBC

### trello module

TBC

### dlrn module

TBC
