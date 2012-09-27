guard :shell, all_on_start: true do
  watch /work_scripts\/.*\.l?hs$/ do |m|
    puts "\n\n\nCompiling..."
    `runghc #{m[0]} && echo "Compiled!"`
  end
end

# vim:ft=ruby
