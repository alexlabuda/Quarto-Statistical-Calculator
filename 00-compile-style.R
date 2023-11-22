

example_file <- system.file("theme-custom.scss", package = "sass")
sass(sass_file(example_file))

sass::sass(sass::sass_file("styles/theme-custom.scss"))
