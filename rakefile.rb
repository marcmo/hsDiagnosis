require 'rake/clean'
require 'benchmark'

Program="diagnosis"
Output="output.out"

MainHs="Main.hs"
TestDir="Test"
Executable = "dist/build/#{Program}/#{Program}"
#Profiling
ProfilingExecutable = "for_profiling"
TimeProf="+RTS -p -K100M"      
StandardHeap="+RTS -hc -p -K100M" 
AllocationType="+RTS -hy -p -K100M" 
ConstructorAlloc="+RTS -hd -p -K100M" 
Profiling=TimeProf
# Profiling=StandardHeap
# Profiling=AllocationType
# Profiling=ConstructorAlloc
CLEAN.include(Output,"**/*.o","**/*.hi","dist","#{Program}.zip","#{ProfilingExecutable}*")
SrcFiles = FileList.new('*.hs')

file Executable => SrcFiles do
  sh "runhaskell Setup.lhs --user configure"
  sh "runhaskell Setup.lhs build"
end
task :build => [Executable]

file ProfilingExecutable => SrcFiles do
  sh "ghc -O2 -o #{ProfilingExecutable} -outputdir tmp --make #{MainHs} -prof -auto-all -caf-all -fforce-recomp"
end

desc "run all quickCheck testcases"
task :test do
  sh 'runhaskell Test/tests.hs'
end

desc "profiling"
task :prof => [:clean,ProfilingExecutable] do
  benchmark = Benchmark.realtime do
    sh "time ./#{ProfilingExecutable} profilinginput #{Output} #{Profiling}"
  end
  puts "computing step took: " + sprintf("%.2f", benchmark)
  if Profiling!=TimeProf
    sh "hp2ps -e8in -c #{ProfilingExecutable}.hp"
  end
end

task :pack do
  sh "zip #{Program}.zip *.cabal *.hs *.rb README *.lhs"
end

task :default => [:clean, :build]

