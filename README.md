## R Helper Functions
#
## HFR#1 plotmat(): Simple plotting using matrix mat (filled with 0/nonzero int).
#  Where: mat - matrix; fn - file name (no extension); clr - color;
#         ttl - plot title; dflg - writing dump file flag (0-no/1-yes):
#         psz - picture size; cx - cex or scale.
plotmat <- function(mat, fn, clr, ttl, dflg=0, psz=600, cx=1.0) {
  m = nrow(mat); d = 0; X=NULL; Y=NULL;
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  # Building X and Y arrays for plotting from not equal to zero values in mat.
  for (i in 1:m) {
    for (j in 1:m) {if(mat[i,j]==0){next} else {d=d+1; X[d]=i; Y[d]=j} }
  };
  cat(" *** Matrix(", m,"x",m,")", d, "DOTS\n");
  # Dumping if requested (dflg=1).
  if (dflg==1) {dump(c("X","Y"), df); cat(" *** Dump file:", df, "\n")};
  # Plotting
  if (ttl!="") {
    plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=c(0,0,0,0));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
}

## HFR#2 plotv2(): Simple plotting using 2 vectors (dumped into ".dmp" file).
# Where: fn - file name; clr - color; ttl - plot title; psz - picture size;
#        cx - cex or scale.
plotv2 <- function(fn, clr, ttl, psz=600, cx=1.0) {
  cat(" *** START:", date(), "clr=", clr, "psz=", psz, "\n");
  pf = paste0(fn, ".png"); df = paste0(fn, ".dmp");
  source(df); d = length(X);
  cat(" *** Plot file -", pf, "Source dump-file:", df, d, "DOTS\n");
  # Plotting
  if (ttl!="") {
    plot(X,Y, main=ttl, axes=FALSE, xlab="", ylab="", col=clr, pch=20, cex=cx)}
  else {par(mar=c(0,0,0,0));
    plot(X,Y, axes=FALSE, xlab=NULL, ylab=NULL, col=clr, pch=20, cex=cx)};
  # Writing png-file
  dev.copy(png, filename=pf, width=psz, height=psz);
  # Cleaning
  dev.off(); graphics.off();
  cat(" *** END:", date(), "\n");
}
