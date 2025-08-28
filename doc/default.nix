{ pkgs, stdenvNoCC, python312Packages }:
let
  mytex = pkgs.texlive.combine {
    inherit (pkgs.texlive)
      biblatex
      enumitem
      glossaries
      latex-bin
      latexmk
      minted
      ninecolors
      pdfcol
      rsfs cm-super
      scheme-small
      svg
      tcolorbox
      tikzfill
      tikz-cd
      titlesec
      transparent
      upquote
      xcolor
      ;
  };
in

stdenvNoCC.mkDerivation {
  name = "haskell numbers";
  pname = "haskell numbers";
  src = ./src;
  nativeBuildInputs = [ mytex python312Packages.pygments pkgs.biber ];
  buildPhase = ''
    latexmk 00-main.tex
  '';
  installPhase = ''
    mkdir -p $out
    mv build/00-main.pdf $out/
  '';
}
