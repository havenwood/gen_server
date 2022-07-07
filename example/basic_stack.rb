# frozen_string_literal: true

require_relative '../lib/gen_server'

module BasicStack
  extend GenServer

  module_function

  # Callbacks

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

# Start the server
GenServer.start_link(BasicStack, [:hello]) => [:ok, pid]

# This is the client
p GenServer.call(pid, :pop)
#=> :hello

p GenServer.cast(pid, %i[push world])
#=> :ok

p GenServer.call(pid, :pop)
#=> :world
