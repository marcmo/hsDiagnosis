require 'rake/clean'
require 'benchmark'

Out="bin"
directory Out
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

if OS.unix?
  def exeName(n) File.join(Out,n) end
else
  def exeName(n) File.join(Out,"#{n}.exe") end
end

MainHs="Main.hs"
LuaMainHs="LuaMain.hs"
DiagScripterHs="DiagScripterMain.hs"
DiagTool = exeName "diagTool"
LuaScripter = exeName "luaexecuter"
DiagScripter = exeName "diag_scripter"

ProfArgs={
  :time=>"+RTS -p -K100M",
  :heap=>"+RTS -hc -p -K100M",
  :allocType=>"+RTS -hy -p -K100M",
  :constructorAlloc=>"+RTS -hd -p -K100M"
}
CLEAN.include(Output,"**/*.o","**/*.hi","dist")
CLOBBER.include(Out)
SrcFiles = FileList.new('**/*.hs')

def buildExe(exe,main)
  puts "building executable:" + exe
  sh "ghc -O2 -o #{exe} -outputdir #{Out} --make #{main} -fforce-recomp -fspec-constr-count=6"
  # sh "ghc -O2 -o #{exe} -outputdir #{Out} --make #{main} -threaded -fforce-recomp -fspec-constr-count=6"
  # stripExec exe
end

file DiagScripter => SrcFiles << Out do
  buildExe(DiagScripter,DiagScripterHs)
end
file DiagTool => SrcFiles << Out do
  buildExe(DiagTool,MainHs)
end
file LuaScripter => SrcFiles << Out do
  buildExe(LuaScripter,LuaMainHs)
end

desc "rebuild all executables"
task :build => [:clean,DiagTool,DiagScripter,LuaScripter]

desc "rebuild diagScripter"
task :scripter => [:clean,DiagScripter]

namespace "lua" do
  desc "build luaScripter"
  task :scripter => [:clean,LuaScripter]
  desc "run lua_scripter"
  task :run, :scriptName do |t,args|
    cd "Lua" do
      sh "../#{LuaScripter} #{args[:scriptName]}"
    end
  end
  task :run => LuaScripter
end

desc "run all testcases"
task :test do
  sh 'runhaskell Tests/testMain.hs'
end

desc "cabal test"
task :cabalTest do
  sh "cabal clean"
  sh "cabal configure --enable-tests"
  sh "cabal build"
  sh "cabal test"
end

desc 'install all cabal dependencies'
task :all_deps do
  sh "sudo cabal install --only-dependencies"
end
task :deps do
  sh "cabal-dev install --only-dependencies"
end
desc "prepare cabal-dev build"
task :dev => :deps do
  sh "cabal-dev clean"
  sh "cabal-dev configure"
  sh "cabal-dev build"
end

def stripExec (x)
  if OS.unix?
    sh "strip #{x}"
    sh "upx #{x}"
  end
end
desc 'execute nvramtest.skr test files'
task :nvram do
  sh "runghc Test/DiagScriptTester.hs \"Script/nvramtest.skr\""
end

task :default => [:clean, :build]

desc "generate tag file"
task :tags do
  sh "find . | egrep '\.hs$' | xargs hothasktags > tags"
end

