\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Cradle-Song of the Blessed Virgin"}}
  poet = \markup\oldStyleNum"Translated by Rev. H.R. Bramley (1833–1917) from Latin"
  composer = \markup\oldStyleNum"Joseph Barnby (1838–1896)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #166
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 6/8
  \autoBeamOff
}

sopMusic = \relative c' {
  \tempo \markup\italic"Allegretto non troppo."
  \partial 8 a'8 |
  a4 a8 c4 a8 |
  a4. g4 a8 |
  bes4 bes8 d4 c8 |
  
  bes4. a4 \bar""\break a8 |
  b4 b8 c4 a8 |
  g4( \acciaccatura b8 a8) g4 g8 |
  
  e'4 e8 d4 c8 |
  c4.( d) |
  \partial 8*5 c( c4) \bar "||" \break
  \partial 8 c8^\markup\italic"piu lento." |
  
  c4 a8 g4 f8 |
  f4 g8 a4 bes8 |
  g4.(~ g |
  \partial 8*5 f~ f4) \bar ":|." \break
  
  
  
  \partial 8 a8 |
  a4 a8 c4 a8 |
  a4. g4 a8 |
  bes4 bes8 d4 c8 |
  
  bes4. a4 \bar"" \break a8 |
  b4 b8 c4 a8 |
  g4( \acciaccatura b8 a8) g4 g8 |
  
  e'4 e8 d4 c8 |
  c4.( d) |
  \partial 8*5 c( c4) \bar "||" \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f8 |
  f4 f8 f4 f8 |
  f4. f4 f8 |
  f4 f8 e4 e8 |
  
  e4. f4 f8 |
  f4 f8 f4 f8 |
  f4. f4 f8 |
  
  e4 e8 f4 fis8 g4.( f) |
  e~ e4 e8 |
  
  f4 f8 ees4 ees8 |
  d4 d8 d4 d8 |
  d4.(\( e\) |
  f~ f4)
  
  
  
  f8 |
  f4 f8 f4 f8 |
  f4. f4 f8 |
  f4 f8 e4 e8 |
  
  e4. f4 f8 |
  f4 f8 f4 f8 |
  f4. f4 f8 |
  
  e4 e8 f4 fis8 g4.( f) |
  e~ e4
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  The Vir -- gin stills the cry -- ing
  Of Je -- sus, sleep -- less ly -- ing;
  And sing -- ing for His plea -- sure,
  Thus calls up -- on her Trea -- sure,
  “My Dar -- ling, do not weep, My Je -- su, sleep!” __
  
  
  
  \set stanza = #"4. "
  My Joy, my Ex -- ul -- ta -- tion,
  My spi -- rit’s Con -- so -- la -- tion;
  My Son, my Spouse, my Bro -- ther,
  O lis -- ten to Thy Mo -- ther!
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  O Lamb, my love in -- vit -- ing,
  O Star, my soul de -- light -- ing,
  O Flow’r of mine own bear -- ing,
  O Jew -- el past com -- par -- ing!
  
  \repeat unfold 10{\skip1}
  
  \set stanza = #"5. "
  Say, wouldst Thou heav’n -- ly sweet -- ness,
  Or love of an -- sw’ring meet -- ness?
  Or is fit mu -- sic want -- ing?
  Ho! An -- gels, raise your chant -- ing!
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  My Child, of Might in -- dwell -- ing,
  My Sweet, all sweets ex -- cell -- ing,
  Of bliss the Foun -- tain flow -- ing,
  The Day -- spring ev -- er glow -- ing,
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  c8 |
  c4 c8 a4 c8 |
  d4. d4 d8 |
  d4 d8 bes4 c8 |
  
  d4( c8) c4 c8 |
  b4 b8 a4 c8 |
  d4( c8) b4 b8 |
  
  c4 c8 d4 ees8 |
  e!4.( b) |
  c( bes!4) bes8 |
  
  a4 a8 c4 c8 |
  c4 bes8 bes4 bes8 |
  bes4.(~ bes |
  a~ a4) 
  
  
  
  c8 |
  c4 c8 a4 c8 |
  d4. d4 d8 |
  d4 d8 bes4 c8 |
  
  d4( c8) c4 c8 |
  b4 b8 a4 c8 |
  d4( c8) b4 b8 |
  
  c4 c8 d4 ees8 |
  e!4.( b) |
  c( bes!4) 
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8 |
  f4 f8 f4 f8 |
  bes4. bes4 a8 |
  g4 g8 c,4 c8 |
  
  f4. f4 e8 |
  d4 d8 c4 c8 |
  b4( c8) d4 g,8 |
  
  c4 c8 a4 a8 |
  g4.~ g |
  c( c4) c8 |
  
  f4 f8 a,4 a8 |
  bes4 bes8 g4 g8 |
  c4.(~ c |
  f~ f4) 
  
  
  
  f8 |
  f4 f8 f4 f8 |
  bes4. bes4 a8 |
  g4 g8 c,4 c8 |
  
  f4. f4 e8 |
  d4 d8 c4 c8 |
  b4( c8) d4 g,8 |
  
  c4 c8 a4 a8 |
  g4.~ g |
  c( c4)
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

