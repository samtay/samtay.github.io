# samtay.github.io [![CircleCI](https://circleci.com/gh/samtay/samtay.github.io.svg?style=svg)](https://circleci.com/gh/samtay/samtay.github.io)

Built on [Chalk](https://github.com/nielsenramon/chalk).

Comments by [utteranc.es](https://utteranc.es/).

Autodeployed by [CircleCI](https://circleci.com/).

Hosted at [samtay.github.io](https://samtay.github.io).

## prereqs
Install

  - [rbenv](https://github.com/rbenv/rbenv#installation)
  - [ruby-build](https://github.com/rbenv/ruby-build#installation) as a plugin to rbenv
  - and npm

## usage
After cloning, get ruby via `rbenv install`. Then run `npm run setup` to gather
dependencies. To serve the site locally at `localhost:4000`, run `npm run
local`.

After adding new gems or npm packages, run `npm run setup` again.
