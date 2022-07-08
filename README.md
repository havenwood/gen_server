# GenServer

A Ruby implementation of GenServer backed by Ractors. See the [Elixir GenServer docs](https://hexdocs.pm/elixir/GenServer.html) and the [Ruby Ractor docs](https://github.com/ruby/ruby/blob/master/doc/ractor.md).

## Example

```ruby
require 'gen_server'

module Stack
  extend GenServer

  module_function

  # Client

  def start_link(default = [])
    GenServer.start_link(self, default)
  end

  def push(pid, element)
    GenServer.cast(pid, [:push, element])
  end

  def pop(pid)
    GenServer.call(pid, :pop)
  end

  # Server (callbacks)

  def init(stack)
    [:ok, stack]
  end

  def handle_call(message, _from, (head, *tail))
    message => :pop

    [:reply, head, tail]
  end

  def handle_cast((cast, element), state)
    cast => :push
    state.unshift(element)

    [:noreply, element]
  end
end

Stack.start_link([:hello]) => [:ok, pid]

p Stack.pop(pid)
#=> :hello

p Stack.push(pid, :world)
#=> :ok

p Stack.pop(pid)
#=> :world
```
Check out the [example/ directory](https://github.com/havenwood/gen_server/tree/main/example).

## Installation

Install it directly.
```sh
gem install gen_server
```

Or add it to your bundle.
```sh
bundle add gen_server
```

