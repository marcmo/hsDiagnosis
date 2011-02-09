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
LuaMainHs="LuaMain.hs"
DiagScripterHs="DiagScripterMain.hs"
if OS.unix?
  DiagTool = "diagtool"
  LuaScripter = "lua_scripter"
  DiagScripter = "diag_scripter"
else
  DiagTool = "diagTool.exe"
  LuaScripter = "lua_scripter.exe"
  DiagScripter = "diag_scripter.exe"
end

TmpFolder = "tmp"
TimeProf="+RTS -p -K100M"      
StandardHeap="+RTS -hc -p -K100M" 
AllocationType="+RTS -hy -p -K100M" 
ConstructorAlloc="+RTS -hd -p -K100M" 
CLEAN.include(TmpFolder,Output,"**/*.o","**/*.hi","dist")
SrcFiles = FileList.new('**/*.hs')

file DiagScripter => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{DiagScripter} -outputdir #{TmpFolder} --make #{DiagScripterHs} -threaded -fforce-recomp"
  stripExec DiagScripter
end
file DiagTool => SrcFiles do
  puts "building executable..."
  sh "ghc -O2 -o #{DiagTool} -outputdir #{TmpFolder} --make #{MainHs} -threaded -fforce-recomp"
  stripExec DiagTool
end
file LuaScripter => SrcFiles do
  puts "building lua-executable..."
  sh "ghc -O2 -o #{LuaScripter} -outputdir #{TmpFolder} --make #{LuaMainHs} -threaded -fforce-recomp"
end

desc "build executable"
task :build => [:clean,DiagTool]

desc "build diagScripter"
task :scripter => [:clean,DiagScripter]

namespace "lua" do
  desc "build luaScripter"
  task :scripter => [:clean,LuaScripter]
  desc "run lua_scripter"
  task :run => LuaScripter do
    cd "Lua" do
      sh "../#{LuaScripter}"
    end
  end
end

desc "run all quickCheck testcases"
task :test do
  sh 'runhaskell tests/testHSFZ.hs'
end

def stripExec (x)
  if OS.unix?
    sh "strip #{DiagTool}"
    sh "upx #{DiagTool}"
  end
end
desc 'execute nvramtest.skr test files'
task :nvram do
  sh "runghc Test/DiagScriptTester.hs \"Script/nvramtest.skr\""
end

task :default => [:clean, :build]

