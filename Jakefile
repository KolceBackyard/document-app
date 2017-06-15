const options = {
  printStdout: true,
  printStderr: true
}

desc('Run the API server.')
task('api', () =>
     jake.exec('json-server --watch database.json', options))

desc('Run Elm Reactor.')
task('elm:dev', () =>
     jake.exec('elm-reactor', options))

desc('Compile Elm files.')
task('build', () =>
     jake.exec('elm-make src/main.elm --output=public/main.js', options))

desc('Watch Elm files and compile on change.')
task('build:watch', () =>
     jake.exec('chokidar "src/**/*.elm" -c "jake build"', options))

desc('Development')
task('default', () => {
  jake.Task['api'].invoke()
  jake.Task['elm:dev'].invoke()
  jake.Task['build:watch'].invoke()
})
