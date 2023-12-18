\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"I Saw Three Ships"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional English (Derbyshire)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #065
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
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
  \override DynamicText.X-offset = #-6
}

sopMusic = \relative c' {
    \repeat volta 2 {
    \partial 8 d8 |
    \slurDashed  g4 g8\noBeam a\noBeam( a\noBeam) b |
    d4 b8 a4 c8 |
    
    b4 g8 g4 b8 |
    a4 fis8 d4 \bar""\break d8 |
    g4 g8\noBeam a\noBeam( a\noBeam) b |
    
    d4 b8 a4 c8 |
    b4 g8\noBeam g\noBeam a\noBeam b |
    \partial 8*5 a4. g4 \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  \slurDashed d4 d8 d\noBeam( d\noBeam) d |
  d4 d8 d4 d8 |
  
  d4 d8 d4 d8 |
  d4 d8 d4 c8 |
  b4 e8 d\noBeam( d\noBeam) g |
  \slurSolid g[ fis] g8 fis4 a8 |
  g4 d8\noBeam e\noBeam fis\noBeam g |
  fis4. g4
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  I saw three ships _ come sail -- ing in,
  On Christ -- mas day, on Christ -- mas day,
  I saw three ships _ come sail -- ing in,
  On Christ -- mas day in the morn -- ing.
  
  
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  O they sailed in -- _ to Beth -- le -- hem,
  On Christ -- mas day, on Christ -- mas day,
  O they sailed in -- _ to Beth -- le -- hem,
  On Christ -- mas day in the morn -- ing.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  And what was in __ _ those ships all three,
  On Christ -- mas day, on Christ -- mas day,
  And what was in __ _ those ships all three,
  On Christ -- mas day in the morn -- ing.
  
  
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
  And all the bells _ on earth shall ring,
  On Christ -- mas day, on Christ -- mas day,
  And all the bells _ on earth shall ring,
  On Christ -- mas day in the morn -- ing.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  The Vir -- gin Ma -- ry and Christ were there,
  On Christ -- mas day, on Christ -- mas day,
  The Vir -- gin Ma -- ry and Christ were there,
  On Christ -- mas day in the morn -- ing.
  
  
  \set stanza = #"7. "
  \set ignoreMelismata = ##t
  And all the An -- gels in Heav’n shall sing,
  On Christ -- mas day, on Christ -- mas day,
  And all the An -- gels in Heav’n shall sing,
  On Christ -- mas day in the morn -- ing.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Pray, whith -- er sailed _ those ships all three,
  On Christ -- mas day, on Christ -- mas day,
  Pray, whith -- er sailed _ those ships all three,
  On Christ -- mas day in the morn -- ing.
  
  \set stanza = #"8. "
  \set ignoreMelismata = ##t
  And all the souls _ on earth shall sing,
  On Christ -- mas day, on Christ -- mas day,
  And all the souls _ on earth shall sing,
  On Christ -- mas day in the morn -- ing.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \repeat unfold 32 \skip1
  \set stanza = #"9. "
  \set ignoreMelismata = ##t
  Then let us all __ _ re -- joice a -- main,
  On Christ -- mas day, on Christ -- mas day,
  Then let us all __ _ re -- joice a -- main,
  On Christ -- mas day in the morn -- ing.
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}
altoWordsVIII = \lyricmode {
}
altoWordsIX = \lyricmode {
}

tenorMusic = \relative c {
  d8 |
  \slurDashed b'4 b8 c\noBeam( c\noBeam) b |
  a4 g8 fis4 a8 |
  
  g4 b8 b4 g8 |
  fis4 a8 fis4 fis8 |
  g4 b8 a\noBeam( a\noBeam) d |
  
  \slurSolid d[ c] b d4 d8 |
  d4 b8 c\noBeam c\noBeam d |
  d4. b4
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  \slurDashed g4 g8 g\noBeam( g\noBeam) g |
  fis4 g8 d4 fis8 |
  
  g4 g8 g4 g8 |
  d4 d8 d4 d8 |
  e4 e8 fis\noBeam( fis\noBeam) g |
  
  \slurSolid b[ a] g d4 fis8 |
  g4 g8 c\noBeam a\noBeam g |
  d'4. g,4
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global {\tempo \markup\italic"Briskly." \sopMusic \sopMusic} >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold2 \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIX"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIX
    \new Lyrics = "altosVIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVIII
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.7)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold2 \bassMusic >> }
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

