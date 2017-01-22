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

### Choose activities that you promise getting them done TODAY

    $ 

### Start Pomodoro(time box)

    $ lyco start

### Take a break

for 5 - 10 min and then try next Pomodoro.

### Report today's statistics

    $ lyco 

Feature
-----------------

### Creating your project

    $ mkdir my-project
    $ cd my-project
    $ lyco init "my-project"
    1
    $ lyco project
    1	my-project

This command creates some project respective configuration in .lyco.yml file.

    $ cat .lyco
    projectId: 1
    projectName: "my-project"

### Sprint in project

List sprints in this project.

    $ lyco sprint
    $ lyco new sprint -n "Planning backlog" -e 2014-12-31
    1

Sprint is a iteration in terms of Agile.
It's a milestone in a nutshell.

if sprint name says something about value to be delivered to user, it's preferable.
Describe not "what you will do" but "what value to be done"

    Planning backlog: until 2014-12-31

The restriction above aims to support reducing overhead of multi-sprint context switch of us.

### Issues in a sprint

Let's check current sprint.

    $ lyco sprint
    Planning backlog: until 2014-12-31 *

asterisk mark indicates that current sprint.
currently implementation will support only single sprint and not multiple sprint at the same time because we should get things done step by step.

Creating new issue.

    $ lyco issue new -s "my first issue" -d "this is description of this issue" -p 1.0
    1
    $ lyco issue new -s "my second issue" -d "this is description of this issue" -p 2.0
    2
    # lyco issue
    1     Planning backlog      1.0      my first issue
    2     Planning backlog      2.0      my second issue

### Inbox

List current opened issues over any projects.

    $ lyco inbox
    1	project-1	sprint-1	1.0	this is a issue.
    2	project-1	sprint-1	2.0	this is also a issue.
    3	project-2	sprint-2	2.0	is this a issue? yes.

### Reporting

T.B.W.

### Planning at the beginning

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


