module Std
  module Ruby
    def self.wrap(block)
      ok = nil
      error = nil

      begin
        ok = block.call
      rescue => e
        error = e.to_s
      end

      if error.nil?
        ::Std::Result::Result::Ok.new(ok)
      else
        ::Std::Result::Result::Err.new(error)
      end
    end
  end
end
