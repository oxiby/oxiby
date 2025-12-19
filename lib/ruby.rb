module Std
  module Ruby
    def self.wrap(block)
      ok = nil
      error = nil

      begin
        ok = block.call
      rescue => e
        error e.to_s
      end

      if error.nil?
        ::Std::Result::Result::Err.new(error)
      else
        ::Std::Result::Result::Ok.new(ok)
      end
    end
  end
end
