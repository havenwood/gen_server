# frozen_string_literal: true

require 'forwardable'
require 'singleton'
require_relative 'registry/info'

module GenServer
  class Registry
    include Singleton
    extend Forwardable

    attr_reader :pids

    def initialize
      @pids = {}
    end

    def_delegators :@pids, :[], :[]=, :fetch, :delete

    class << self
      def [](pid)
        instance[pid]
      end

      def []=(pid, info)
        instance[pid] = info
      end

      def fetch(...)
        instance.fetch(...)
      end

      def delete(pid)
        instance.delete(pid)
      end
    end
  end
end
