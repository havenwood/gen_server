# frozen_string_literal: true

require_relative 'test_helper'

class Noop
  include GenServer
end

class BasicStack
  include GenServer

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

describe GenServer do
  describe 'its version' do
    it 'exists' do
      assert GenServer::VERSION
    end

    it 'has three parts' do
      assert_equal 3, GenServer::VERSION.split('.').size
    end
  end

  describe 'start_link' do
    it 'returns a pid' do
      GenServer.start_link([]) => [:ok, pid]

      assert pid
    end
  end

  describe 'a noop GenServer' do
    before do
      GenServer.start_link(Noop, []) => [:ok, pid]
      @noop_pid = pid
    end

    it 'calls and casts' do
      assert_nil GenServer.call(@noop_pid, :foo)
      assert_equal :ok, GenServer.cast(@noop_pid, %i[push bar])
      assert_nil GenServer.call(@noop_pid, :foo)
    end
  end

  describe 'a basic stack GenServer' do
    before do
      GenServer.start_link(BasicStack, [:hello]) => [:ok, pid]
      @pid = pid
    end

    it 'pushes and pops' do
      assert_equal :hello, GenServer.call(@pid, :pop)
      assert_equal :ok, GenServer.cast(@pid, %i[push world])
      assert_equal :world, GenServer.call(@pid, :pop)
    end
  end
end
