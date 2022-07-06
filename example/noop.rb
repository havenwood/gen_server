# frozen_string_literal: true

require_relative '../lib/gen_server'

class Noop
  include GenServer
end

GenServer.start_link(Noop, []) => [:ok, pid]
#=> nil

p GenServer.call(pid, :foo)
#=> nil

p GenServer.cast(pid, %i[push bar])
#=> :ok

p GenServer.call(pid, :foo)
#=> nil
