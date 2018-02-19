# sbot: Install

This is an ansible-playbook to help you get started with sbot deployment.
It will pull all the required (system and prolog) dependencies.

This playbook has been run/tested on a Fedora 27 OS.

The `hosts` file looks like:

```
[botsito]
botsito.example.com ansible_python_interpreter=/usr/bin/python3
```

Then simply run: `ansible-playbook -i hosts install-playbook.yml -e @settings.yml`

At the end of the run, you will have sbot running on a tmux session on your target host.
To attach to it run: `tmux attach -t botsito`. The output of the bot will be available.
You'll be in the prolog REPL.
