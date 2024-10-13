\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Stille Nacht"}}
  poet = \markup\oldStyleNum"Joseph Mohr (1792–1848)"
  composer = \markup\oldStyleNum"Franz Gruber (1787–1863)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
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
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #034
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key bes \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo \markup\italic"Tranquillo" 8 = 90
  \slurDotted 
  f8.[ g16] f8 d4. |
  f8. g16 f8 d4. |
  c'8.[ b16] c8 a4. | \break
  bes8.[ a16] bes8 f4. |
  
  g4 g8 bes8.( a16) g8 |
  f8.\noBeam g16\noBeam f8 d4 bes'8\rest | \break
  g4 g8 bes8.( a16) g8 |
  f8.\noBeam( g16\noBeam) f8 d4 bes'8\rest |
  
  \slurSolid c4 c8\noBeam ees8.\noBeam c16\noBeam a8 | \break
  bes4.( d4) bes8\rest |
  bes8[ f] d8 f8.\noBeam ees16\noBeam c8 |
  bes4.~bes4 bes'8\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDotted 
  d8.[ ees16] d8 bes4. |
  d8. ees16 d8 bes4. |
  ees8.[ d16] ees8 c4. |
  d8.[ c16] d8 d4. |
  
  ees4 ees8 g8.( f16) ees8 |
  d8.\noBeam ees16\noBeam d8\noBeam bes4 s8 |
  ees4 ees8 g8.( f16) ees8 |
  d8.\noBeam( ees16\noBeam) d8 bes4 s8 |
  
  \slurSolid ees4 ees8 c8.\noBeam ees16\noBeam c8 |
  d4.( f4) s8 |
  d4 bes8 d8.\noBeam c16\noBeam a8 |
  bes4.~ bes4 s8 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Stil -- le Nacht!
  hei -- li -- ge Nacht!
  Al -- les schläft;
  ein -- sam wacht.
  Nur das
  \set ignoreMelismata = ##t
  trau -- te hoch -- hei -- li -- ge Paar. 
  Hol -- der Kna -- be im lock -- i -- gen Haar,
  \unset ignoreMelismata
  Schlaf in himm -- li -- scher Ruh! __ 
  Schlaf in himm -- li -- scher Ruh! __
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup\concat{\dynamic"p   " "2. "}
  \lyricmode {
    Stil -- le Nacht!
    hei -- li -- ge Nacht!
    Hir -- ten erst kund -- ge -- macht,
    Durch der En -- gel Hal -- le -- lu -- ja!
    Tönt es laut __ von fern __ und nah:
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    Christ, der Ret -- ter ist da! __
  }
  \set stanza = \markup\dynamic"pp      "
  \lyricmode {
    Christ, der Ret -- ter ist da! __
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Stil -- le Nacht!
  hei -- li -- ge Nacht!
  Got -- tes Sohn, o wie lacht
  Lieb’ aus dein -- em göt -- tlich -- en Mund,
  Da uns schlägt die
  \set ignoreMelismata = ##t
  ret -- ten -- de Stund’. 
  \unset ignoreMelismata
  Christ, in dein -- er Ge -- burt! __
  Christ, in dein -- er Ge -- burt! __
}
altoWordsIV = \lyricmode {
\set ignoreMelismata = ##t
%\markup\italic 
  
}

tenorMusic = \relative c' {
  \slurDotted 
  bes4 bes8 f4. |
  bes8. bes16 bes8 f4. |
  a4 a8 f4. |
  f4 f8 bes4. |
  
  bes4 bes8 g8.( a16) bes8 |
  bes8.\noBeam bes16\noBeam bes8 f4 s8 |
  bes4 bes8 g8.( a16) bes8 |
  bes8.\noBeam( bes16\noBeam) bes8 f4 s8 |
  
  a4 a8\noBeam a8.\noBeam a16\noBeam f8 |
  \slurSolid f4.( bes4) s8 |
  f4 f8\noBeam f8.\noBeam f16\noBeam ees8 |
  d4.~d4 s8 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDotted 
  bes4 bes8 bes4. |
  bes8. bes16 bes8 bes4. |
  f'4 f8 f4. |
  bes,4 bes8 bes4. |
  
  ees4 ees8 ees8.( ees16) ees8 |
  bes8.\noBeam bes16\noBeam bes8\noBeam bes4 d8\rest |
  ees4 ees8 ees8.( ees16) ees8 |
  bes8.\noBeam( bes16\noBeam) bes8 bes4 d8\rest |
  
  f4 f8\noBeam f8.\noBeam f16\noBeam f8 |
  bes,4.~ bes4 d8\rest |
  f,4 f8\noBeam f8.\noBeam f16\noBeam f8 |
  bes4.~ bes4 d8\rest \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
