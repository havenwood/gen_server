# frozen_string_literal: true

require_relative '../lib/gen_server'

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
