# frozen_string_literal: true

require_relative '../lib/gen_server'

module Periodically
  extend GenServer

  module_function

  def start_link(data)
    GenServer.start_link(Periodically, data)
  end

  def init(state)
    # Schedule work to be performed on start
    schedule_work

    [:ok, state]
  end

  def handle_info(info, state)
    info => :work
    # Do the desired work here
    # ...

    # Reschedule once more
    schedule_work

    [:noreply, state]
  end

  def schedule_work
    # We schedule the work to happen in 2 seconds (written in milliseconds).
    GenServer.send_after(self, :work, 2 * 1_000)
  end
end

Periodically.start_link({}) => [:ok, pid]
