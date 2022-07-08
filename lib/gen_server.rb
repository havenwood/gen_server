# frozen_string_literal: true

require_relative 'gen_server/pid'
require_relative 'gen_server/registry'
require_relative 'gen_server/version'

module GenServer
  def init(state) = [:ok, state]
  def handle_cast(_message, _state) = [:noreply, nil]
  def handle_call(_message, _sender, _state) = [:reply, nil, nil]
  def terminate(_reason, _state) = [:stop]

  class << self
    def start_link(receiver, init_state = [], name: nil)
      Object.const_set(name, receiver) if name

      pid = PID.new
      receiver.define_singleton_method(:pid) { pid }

      actor = Ractor.new(init_state, name: pid.to_s) do |init_state|
        GenServer.receive(init_state)
      end

      Registry[pid] = Registry::Info.new(actor:, receiver:)
      receiver.init(init_state) => [:ok, state]
      actor.send([:initial, state])

      [:ok, pid]
    end

    def receive(state)
      case Ractor.receive
      in [:initial, state]
        new_state = state
      in [:cast, message, receiver]
        receiver.handle_cast(message, state) => [:noreply, new_state]
      in [:call, sender, message, receiver]
        receiver.handle_call(message, sender, state) => [:reply, reply, new_state]
        sender.send [:ok, reply]
      in [:info, message, receiver]
        receiver.handle_info(message, state) => [:noreply, new_state]
      in [:stop, reason, receiver]
        receiver.terminate(reason, state) => [:stop]
      end

      receive(new_state)
    end

    def cast(pid, message)
      Registry.fetch(pid).values => [actor, receiver]
      actor.send [:cast, message, receiver]

      :ok
    end

    def call(pid, message)
      Registry.fetch(pid).values => [actor, receiver]
      actor.send [:call, Ractor.current, message, receiver]
      Ractor.receive => [:ok, response]

      response
    end

    def stop(pid, reason)
      Registry.fetch(pid).values => [actor, receiver]
      actor.send [:stop, reason, receiver]
      Registry.delete(pid)

      :ok
    end

    def send(pid, message)
      Registry.fetch(pid).values => [actor, receiver]
      #!> schedule_work': defined with an un-shareable Proc in a different Ractor (RuntimeError)
      actor.send [:info, message, receiver]

      :ok
      
    end

    def send_after(pid, message, delay)
      sleep delay.fdiv(1_000)
      send(pid, message)
    end
  end
end
