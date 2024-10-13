\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Carol of the Birds"}}
    poet = \markup\oldStyleNum"Traditional"
    composer = \markup\oldStyleNum"Bas-Quercey Carol"
    tagline = \markup \concat{ "from " \italic"Carols Old and Carols New" \oldStyleNum", 1916, via " \italic"HymnsAndCarolsOfChristmas.com" }
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 0))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #089
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
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \tempo \markup\italic"Not slow."
  g4 f8\noBeam g a4 a |
  \slurDotted bes bes8\noBeam( bes) a2 |
  
  g4 f8\noBeam g a4 g |
  f e d2 |
  
  g4 f8\noBeam g a4 a |
  bes bes a2 |
  
  d4 d8\noBeam c d4 c |
  bes a g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 d8\noBeam d f4 f |
  \slurDotted f f8\noBeam( f) f2 |
  
  d4 d8\noBeam d f4 d |
  d cis d2 |
  
  d4 d8\noBeam d f4 f |
  f f f2 |
  
  f4 f8\noBeam f f4 \slurSolid ees8[ g] |
  g4 fis d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Whence comes this rush of wings a -- _ far,
  Fol -- low -- ing straight the No -- ël star?
  Birds from the woods in won -- drous flight,
  Beth -- le -- hem seek this _ Ho -- ly Night.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  “Tell us, ye birds, why come ye __ _ here,
  In -- to this sta -- ble, poor and drear?”
  “Hast -- ’ning we seek the new -- born King,
  And all our sweet -- est _ mu -- sic bring.”
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Hark how the Green -- finch bears his __ part,
  Phi -- lo -- mel, too, with ten -- der heart,
  Chants from her leaf -- y dark re -- treat
  \markup\italic Re, \markup\italic mi, \markup\italic fa, \markup\italic sol, in ac -- cents sweet.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  An -- gels and shep -- herds, birds of the sky,
  Come where the Son of God doth lie;
  Christ on the earth with man doth dwell,
  Join in the shout, “No -- _ ël, No -- ël!”
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  bes4 a8\noBeam bes c4 c |
  \slurDotted d d8\noBeam( d) c2 |
  
  bes4 bes8\noBeam bes c4 bes |
  a \slurSolid a8[ g] f2 |
  
  \slurDotted bes4 a8\noBeam bes c4 c |
  d d c2 |
  
  bes4 bes8\noBeam a bes4 ees |
  d \slurSolid d8[ c] bes2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 d8\noBeam g f4 f |
  \slurDotted bes bes8\noBeam( bes) f2 |
  
  g4 bes8\noBeam g f4 g |
  a a, d2 |
  
  g4 d8\noBeam g f4 f |
  bes bes f2 |
  
  bes4 bes8\noBeam f bes4 c |
  d d, g2 \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
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
%6x9  \context {\Lyrics\override LyricText #'font-size = #0.75 }
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.25 }
%8.5x11g \context {\Lyrics\override LyricText #'font-size = #1.1 }
%{IF_LESSER
\context {\Lyrics\override LyricText #'font-size = #0.6 }
%}%END_IF_LESSER
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.6 20)))
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
    \tempo 4 = 110
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
