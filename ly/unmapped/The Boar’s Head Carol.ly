\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Boar’s Head Carol"}}
    poet = \markup\oldStyleNum"15th Century English"
    composer = \markup\oldStyleNum"Traditional English"
    tagline = ""
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #076
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
%IF_NOT_LESSER

\markup\fill-line{\concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}}
\markup\vspace#2

%END_IF_NOT_LESSER

















%Boars Head Carol
global = {
  \key c \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c'' {
  \partial 4 g4 |
  \slurDotted c4( c) c4( c8) c8 |
  b4 c g4 \parenthesize e8\noBeam e |
  
  f4 f a4. f8 |
  g4. g8 c4 \bar""\break g8\noBeam( g) |
  c4 c8\noBeam( c) c4 c |
  
  b c g4. e8 |
  f4 f a4. f8 |
  g4. g8 c2 | \break
  
  \repeat volta 2 {
    c4. c8 b4 b |
    c c g2 |
    f4 f a4. f8 |
    g4. g8 c4\fermata
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  \slurDotted e( e) e4( e8) e8 |
  g4 g e4 e8\noBeam e |
  
  f4 f f4. c8 |
  \slurSolid f8[ e] d4 e \slurDotted e8\noBeam( e) |
  e4 e8\noBeam( e) e4 e |
  
  g g e4. e8 |
  f4 f f4. c8 |
  \slurSolid f[ e] d4 e2 |
  
  \repeat volta 2 {
    e4. e8 d4 d |
    e e d2 |
    c4 c c4. d8 |
    \slurSolid d[ e] f4 e |
  }
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1."
    \set ignoreMelismata = ##t
    The boar’s _ head _ in hand bear I "" Be -- decked with bays and rose -- ma -- ry;
    \set associatedVoice = "basses"
    And I pray you my mas -- ters mer -- ry be;
    \markup\italic Quot _ \markup\italic es -- \markup\italic tis _ \markup\italic in \markup\italic con -- \markup\italic vi -- \markup\italic vi -- \markup\italic o.
  }
  \set stanza = \markup\dynamic"  ff - pp"
  \lyricmode {
    \unset ignoreMelismata
    \markup\italic Ca -- \markup\italic put \markup\italic a -- \markup\italic pri \markup\italic de -- \markup\italic fe -- \markup\italic ro
    \set associatedVoice = "tenors"
    \markup\italic Red -- \markup\italic dens
    \set associatedVoice = "basses"
    \markup\italic lau -- \markup\italic des \markup\italic Do -- \markup\italic mi -- \markup\italic no.
  }
}
altoWordsII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"f " "2."}
  \lyricmode {
  %\markup\italic
    \set ignoreMelismata = ##t
    The boar’s _ head as I un -- der -- stand Is the rar -- est dish in all this land,
    \set associatedVoice = "basses"
    Which is thus be -- _ decked with_a gay gar -- land,
    Let _ us
    \markup\italic ser -- _ \markup\italic vi -- \markup\italic re \markup\italic can -- \markup\italic ti -- \markup\italic co.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3."
  \set ignoreMelismata = ##t
  Our stew -- ard hath _ pro -- vid -- ed this
  "" In hon -- or of the King of bliss,
  \set associatedVoice = "basses"
  Which _ on this __ _ day to_be serv -- ed is,
  \markup\italic In _ \markup\italic Re -- \markup\italic gi -- _ \markup\italic nen -- \markup\italic si \markup\italic a -- \markup\italic tri -- \markup\italic o.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  c4 |
  \slurDotted g( g) g4( g8) g8 |
  d'4 e c4 c8\noBeam c |
  
  c4 c c4. a8 |
  c4 b c c8\noBeam( c) |
  g4 g8\noBeam( g) g4 g |
  
  d' e c4. c8 |
  c4 c c4. a8 |
  c4 b c2 |
  
  \repeat volta 2 {
    g4. g8 g4 g |
    g a \slurSolid c( b) |
    a a a4. a8 |
    c4 b c |
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  c4 |
  \slurDotted c( c) c4( c8) c8 |
  g'4 c, c4 c'8\noBeam( bes) |
  
  a4 \slurSolid a8[ g] f4. f8 |
  g4 g c, \slurDotted c8\noBeam( c) |
  c4 c8\noBeam( c) c4 c |
  
  g' c, c'4 c8[ bes] |
  a4 \slurSolid a8[ g] f4 f |
  g4 g c,2 |
  
  \repeat volta 2 {
    c4. c8 g'4 g |
    c, a' g2 |
    a4 a8[ g] f[ e] d4 |
    g4 g, c\fermata |
  }
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "tenors" \altoWords
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
  
  
}

\score {
  \unfoldRepeats

  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "tenors" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  
  \midi {
    \tempo 4 = 150
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

