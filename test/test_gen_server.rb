# frozen_string_literal: true

require 'test_helper'

class TestGenServer < Minitest::Test
  def test_there_is_a_version
    refute_nil ::GenServer::VERSION
  end

  def test_the_version_has_three_parts
    assert_equal 3, ::GenServer::VERSION.split('.').size
  end
end
