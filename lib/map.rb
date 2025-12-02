class Map < Hash
  def [](key)
    value = super

    if value.nil?
      ::Std::Option::Option::None.new
    else
      ::Std::Option::Option::Some.new(value)
    end
  end
end
