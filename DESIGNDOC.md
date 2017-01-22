# Design doc

Private scrum + pomodoro

No teams, no users.

## Supported usecases

Supporting trying pomodoro technique.
Recording activities.

### Overview

* Starting project
    + Add new project
    + Prepare project setting with git
* Sprint planning
    + Add tasks (or backlog)
    + Estimate story points
    + Add new sprint
    + Arrange sprint backlog
* Run sprint
    + Daily routine
        - Choose today's tasks (TODO) from kanban
        - Choose a working task
        - Repeat working pomodoro and taking a break
        - Retrospect at the end of the day
          - Interruption
        - Change the task status (open, work in progress, closed)
    + View burn down chart
* Sprint retrospective
    + View velocity in the sprint
    + Back to next sprint planning
* Closing project

## Git integration usecases

Ummm....it seems that an exec file for client is not neccesarily.
It is smarter to me to provide only bash scripts as commit hook.

### Overview

* Git init
* Lyco init
    + Attach git project with Lyco project by project id
        - Create `.lyco` directory
    + Generate commit hook scripts in `.git`
* Create branch
    + By existing sprint
        - `lyco sprint ${ID}`
    + By existing task
        - `lyco issue ${ID}`
* Commit
    + One commit includes many pomodoro records
* Merge commits into sprint
    + One merge request includes many commits so more records
    + Merging marks issues as closed


## Model

### Project

Means activities to achieve product, service.

### Issue

Means task which has lifecycle.

### Stage (issue status)

Kanban columns.

### Record

Pomodoro activity which has start time and end time

### Sprint

Means milestone which is defined by start date and due date.

