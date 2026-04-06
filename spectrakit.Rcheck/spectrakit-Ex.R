pkgname <- "spectrakit"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "spectrakit-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('spectrakit')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("combineSpectra")
### * combineSpectra

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: combineSpectra
### Title: Combine and Normalize Spectral Data from Multiple Files
### Aliases: combineSpectra

### ** Examples

# Create a temporary directory for mock CSV files
tmp_dir <- tempdir()

# Define file paths
tmp1 <- file.path(tmp_dir, "file1.csv")
tmp2 <- file.path(tmp_dir, "file2.csv")

# Write two mock CSV files in the temporary folder
write.csv(data.frame(ID = c("A", "B", "C"), val = c(1, 2, 3)), tmp1, row.names = FALSE)
write.csv(data.frame(ID = c("A", "B", "C"), val = c(4, 5, 6)), tmp2, row.names = FALSE)

# Merge the CSV files in the temporary folder, normalize with z-score, and return transposed
result <- combineSpectra(
  folder = tmp_dir,
  file_type = "csv",
  sep = ",",
  common_col_pos = 1,
  data_col_pos = 2,
  normalization = "z-score",
  orientation = "rows"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("combineSpectra", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("makeComposite")
### * makeComposite

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: makeComposite
### Title: Create a labeled image grid
### Aliases: makeComposite

### ** Examples

library(magick)

tmp_dir <- file.path(tempdir(), "spectrakit_imgs")
dir.create(tmp_dir, showWarnings = FALSE)

# Create and save img1
img1 <- image_blank(100, 100, "white")
img1 <- image_draw(img1)
symbols(50, 50, circles = 30, inches = FALSE, add = TRUE, bg = "red")
dev.off()
img1_path <- file.path(tmp_dir, "img1.png")
image_write(img1, img1_path)

# Create and save img2
img2 <- image_blank(100, 100, "white")
img2 <- image_draw(img2)
rect(20, 20, 80, 80, col = "blue", border = NA)
dev.off()
img2_path <- file.path(tmp_dir, "img2.png")
image_write(img2, img2_path)

# Create composite
makeComposite(
        folder = tmp_dir,
        custom_order = c("img1.png", "img2.png"),
        rows = 1,
        cols = 2,
        labels = list(c("Red Circle", "Blue Rectangle")),
        label_settings = list(
                list(size = 5, font = "Arial", color = "black", boxcolor = "white",
                     gravity = "northwest", location = "+10+10", weight = 400)
        ),
        resize_mode = "none",
        desired_width = 10,
        width_unit = "cm",
        ppi = 300,
        output_format = "png",
        output_folder = tmp_dir
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("makeComposite", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotPCA")
### * plotPCA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotPCA
### Title: Perform PCA and Create a Plot of Scores, Loadings, or Biplot
### Aliases: plotPCA

### ** Examples

plotPCA(
  data = iris,
  color_var = "Species",
  shape_var = "Species",
  plot_type = "biplot",
  palette = "Dark2",
  show_labels = TRUE,
  ellipses = TRUE,
  display_names = TRUE,
  legend_title = "Iris Species"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotPCA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotSpectra")
### * plotSpectra

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotSpectra
### Title: Plot Spectral Data from Multiple Files
### Aliases: plotSpectra

### ** Examples

# Create a temporary directory and write mock spectra files
tmp_dir <- tempdir()
write.csv(data.frame(Energy = 0:30, Counts = rpois(31, lambda = 100)),
          file.path(tmp_dir, "spec1.csv"), row.names = FALSE)
write.csv(data.frame(Energy = 0:30, Counts = rpois(31, lambda = 120)),
          file.path(tmp_dir, "spec2.csv"), row.names = FALSE)

# Plot the mock spectra using various configuration options
plotSpectra(
  folder = tmp_dir,
  file_type = "csv",
  sep = ",",
  normalization = "min-max",
  x_config = c(0, 30, 5),
  x_reverse = FALSE,
  y_trans = "linear",
  x_label = expression(Energy~(keV)),
  y_label = expression(Counts/1000~s),
  line_size = 0.7,
  palette = c("black","red"),
  plot_mode = "overlapped",
  display_names = TRUE,
  vertical_lines = c(10, 20),
  shaded_ROIs = list(c(12, 14), c(18, 22)),
  output_format = "png",
  output_folder = tmp_dir
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotSpectra", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
