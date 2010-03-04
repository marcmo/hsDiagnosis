require 'rake/clean'
require 'benchmark'

Output="output.out"

MainHs="Main.hs"
DiagScripterHs="DiagScripterMain.hs"
Executable = "diagnosis"
DiagScripter = "diagnosis2"
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
CLEAN.include(Output,"**/*.o","**/*.hi","dist",Executable,DiagScripter,"#{ProfilingExecutable}*")
SrcFiles = FileList.new('**/*.hs')

file DiagScripter => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{DiagScripter} -outputdir tmp --make #{DiagScripterHs} -threaded -fforce-recomp"
end
file Executable => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{Executable} -outputdir tmp --make #{MainHs} -threaded -fforce-recomp"
  # sh "runhaskell Setup.lhs --user configure"
  # sh "runhaskell Setup.lhs build"
end
desc "build executable"
task :build => [:clean,Executable]
desc "build diagScripter"
task :diagscripter => [:clean,DiagScripter]

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

task :default => [:clean, :build]

