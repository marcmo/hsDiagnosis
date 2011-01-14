require 'rake/clean'
require 'benchmark'

Output="output.out"
module OS
  def OS.windows?
    (RUBY_PLATFORM =~ /cygwin|mswin|mingw|bccwin|wince|emx/) != nil
  end
  def OS.mac?
    (RUBY_PLATFORM =~ /darwin/) != nil
  end
  def OS.unix?
    !OS.windows?
  end
  def OS.linux?
    OS.unix? and not OS.mac?
  end
end

MainHs="Main.hs"
LuaMainHs="Lua_haskell.hs"
DiagScripterHs="DiagScripterMain.hs"
if OS.unix?
  Executable = "diagnosis"
  LuaScripter = "lua_scripter"
  DiagScripter = "diag_scripter"
else
  Executable = "diagnosis.exe"
  LuaScripter = "lua_scripter.exe"
  DiagScripter = "diag_scripter.exe"
end

TmpFolder = "tmp"
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
CLEAN.include(TmpFolder,Output,"**/*.o","**/*.hi","dist")
CLOBBER.include(Executable,DiagScripter,LuaScripter,"#{ProfilingExecutable}*")
SrcFiles = FileList.new('**/*.hs')

file DiagScripter => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{DiagScripter} -outputdir #{TmpFolder} --make #{DiagScripterHs} -threaded -fforce-recomp"
  stripExec DiagScripter
end
file Executable => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{Executable} -outputdir #{TmpFolder} --make #{MainHs} -threaded -fforce-recomp"
  stripExec Executable
end
file LuaScripter => SrcFiles do
  puts "building lua-executable..."
  sh "ghc -O2 -o #{LuaScripter} -outputdir #{TmpFolder} --make #{LuaMainHs} -threaded -fforce-recomp"
end
desc "build executable"
task :build => [:clean,Executable]
desc "build diagScripter"
task :scripter => [:clean,DiagScripter]
namespace "lua" do
  desc "build luaScripter"
  task :scripter => [:clean,LuaScripter]
  desc "run lua_scripter"
  task :run => LuaScripter do
    sh "./#{LuaScripter}"
  end
end

file ProfilingExecutable => SrcFiles do
  sh "ghc -O2 -o #{ProfilingExecutable} -outputdir #{TmpFolder} --make #{MainHs} -prof -auto-all -caf-all -fforce-recomp"
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

def stripExec (x)
  if OS.unix?
    sh "strip #{Executable}"
    sh "upx #{Executable}"
  end
end
desc 'execute nvramtest.skr test files'
task :nvram do
  sh "runghc Test/DiagScriptTester.hs \"Script/nvramtest.skr\""
end

task :default => [:clean, :build]

