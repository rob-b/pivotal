# pivotal

## Installation
While `pivotal` is unavailable on [Hackage](http://hackage.haskell.org/), the
simplest way is to install with [stack](https://docs.haskellstack.org). Simply
clone this repo and run `stack build --copy-bins`.

## Configuration

You will need to get an API token from your [Pivotal profile
page](https://www.pivotaltracker.com/profile). You can either pass this token
to every command as an option e.g. `pivotal --token XXX` or set it as an
environment variable `PIVOTAL_TOKEN`. Likewise, the useful commands all need a
project id to operate on and so you can either pass this as an option,
`pivotal --project-id 11111111` or set it as an environment variable
`PIVOTAL_PROJECT_ID`.

## Usage

~~~
Usage: pivotal [--project-id PROJECTID] [--pivotal-token TOKEN] COMMAND
  Interact with pivotal tracker

Available options:
  -h,--help                Show this help text
  --project-id PROJECTID   Project id
  --pivotal-token TOKEN    Pivotal API token

Available commands:
  stories                  View story
  todo                     View unstarted stories
  started                  View started stories
  finished                 View finished stories
  profile                  View user's profile
  projects                 View user's projects
~~~
