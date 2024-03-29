# frozen_string_literal: true

require_relative 'test_helper'

module Noop
  extend GenServer
end

module BasicStack
  extend GenServer

  module_function

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

  def stop(pid)
    GenServer.stop(pid, :normal)
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

  def terminate(_reason, state)
    state.clear

    [:stop]
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

  describe 'a stack GenServer' do
    before do
      GenServer.start_link(Stack, [:hello], name: :MyStack) => [:ok, pid]
      @pid = pid
    end

    it 'pushes and pops' do
      assert_equal :hello, Stack.pop(@pid)
      assert_equal :ok, Stack.push(@pid, :world)
      assert_equal :world, Stack.pop(@pid)
    end

    it 'aliases the stack by name' do
      assert_equal :hello, MyStack.pop(@pid)
      assert_equal :ok, MyStack.push(@pid, :world)
      assert_equal :world, MyStack.pop(@pid)
    end

    it 'terminates' do
      assert_equal :ok, Stack.stop(@pid)
      assert_nil GenServer::Registry[@pid]
    end
  end
end
