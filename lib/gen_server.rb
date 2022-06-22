# frozen_string_literal: true

require 'securerandom'
require 'singleton'
require_relative 'gen_server/registry'
require_relative 'gen_server/version'

module GenServer
  def initialize(...)
    init(...)
  end

  def init(...) = nil
  def handle_call(...) = [:reply, nil, nil]
  def handle_cast(...) = [:noreply, nil]

  class << self
    def start_link(klass, state = [])
      pid = SecureRandom.uuid

      actor = Ractor.new(state, name: pid) do |state|
        GenServer.receive(state)
      end

      Registry[pid] = Registry::Info.new(actor:, klass:)

      [:ok, pid]
    end

    def receive(state)
      case Ractor.receive
      in [:cast, message, klass]
        klass.allocate.handle_cast(message, state) => [:noreply, new_state]
        receive(new_state)
      in [:call, sender, message, klass]
        klass.allocate.handle_call(message, sender, state) => [:reply, reply, new_state]
        sender.send [:ok, reply]
        receive(new_state)
      end
    end

    def cast(pid, message)
      Registry.actor(pid).send [:cast, message, Registry.klass(pid)]

      :ok
    end

    def call(pid, message)
      Registry.actor(pid).send [:call, Ractor.current, message, Registry.klass(pid)]
      Ractor.receive => [:ok, response]

      response
    end
  end
end
