Lycopene
=================

A command line tool for tracking activities with Pomodoro Technique.

Getting started
-----------------

### Installation

Download the executable binary and run it.

    $ curl -O <url of released binary>
    $ ./lyco version
    1.0.0
    $ ./lyco configure
    /home/alice/.lyco

### Configuration

Pomodoro Technique
-----------------

[About Pomodoro Technique](http://pomodorotechnique.com/)

### Prepare

The following command creates database and other related stuff in your $HOME directory.

    $ lyco configure
    /home/alice/.lyco

### Add your activities planned to get done

    $ lyco add -e 1.0 "Do something"
    1	inbox	no-sprint	1.0	Do something

This means that new activity "Do something" added to `no-sprint` sprint of `inbox` project.

### Start Pomodoro(time box)

    $ lyco start

### Take a break

for 5 - 10 min and then try next Pomodoro.

### Report today's statistics

    $ lyco 

Feature
-----------------

### Inbox

List current opened issues over any projects.

    $ lyco inbox
    1	project-1	sprint-1	1.0	this is a issue.
    2	project-1	sprint-1	2.0	this is also a issue.
    3	project-2	sprint-2	2.0	is this a issue? yes.

### Planning at the beginning

    $ lyco todo -a 1

You can add estimated number of pomodoro

    $ lyco todo -a 1 -e 4

### Start pomodoro

    $ lyco pomo -i 1

### Update estimation

    $ lyco a

### Reporting

T.B.W.

### Track yourself

T.B.W.

Command line cheat sheat
------------------------

| verb | Project | Sprint | Issue | Record |
|------|---------|--------|-------|--------|
| ls   |         |        |       |        |
| new  |         |        |       |        |
| mod  |         |        |       |        |


