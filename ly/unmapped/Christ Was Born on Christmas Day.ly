\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Christ Was Born on Christmas Day"}}
    poet = \markup\oldStyleNum"John Mason Neale (1818–1866)"
    composer = \markup\concat{\oldStyleNum"14th Century German melody, " \italic"Resonet in laudibus"}
    arranger = \markup\oldStyleNum"Arranged chiefly by G. R. Woodward (1848–1934)"
    tagline = \markup \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #036
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
  \key f \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \repeat volta 2{
    c4 a8 f4 a8 |
    c4 d8 c4. |
    c4 a8 f4 a8 |
    c4 d8 c4. |
    
    bes4 bes8 bes4 c8 |
    bes4 a8 g4 a8 |
    c4 a8 f4 bes8 |
    a4 g8 g4 a8 |
    f4. f\fermata \break
  }
  \repeat volta 2{
    c'4 a8 f4 a8 |
    c4 d8 c4. |
    c4 a8 f4 a8 |
    c4 d8 c4. |
    
    bes4 bes8 bes4 c8 |
    bes4 a8 g4 a8 |
    c4 a8 f4 bes8 |
    
    
    %page2
    a4 g8 g4 a8 |
    f4. f\fermata \break
  }
  
  f4 g8 a4 bes8 |
  a4 g8 a4 g8 |
  
  f4 e8 f4. |
  c'4. a |
  c a |
  f4 g8 a4 bes8 |
  
  a4 g8 a4. |
  f4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  g4 g8 f4.\fermata \bar"||"
  
  c'4 c8 d4 e8 |
  f4 e8 d4 c8 |
  f4 e8 d4 c8 |
  c4 b8 c4. |
  
  f,4 g8 a4 bes8 c4 bes8 a4 bes8 |
  g4 g8 f4\fermata \bar"||"
  c'8 |
  c4 a8 f4 a8 |
  
  %page3
  c4 d8 c4\fermata a8 |
  c4\< d8\< c4\<\fermata f,8 |
  f4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  
  g4 g8 f4.\fermata \bar"||"
  c'4 c8 d4 e8 f4 e8 d4 c8 |
  f4 e8 d4 c8 |
  
  c4 b8 c4. |
  f,4 g8 a4 bes8 |
  c4 bes8 a4 bes8 |
  g4 g8 f4.\fermata \bar"|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat volta 2{
    f4 f8 f4 f8 |
    f4 f8 f4. |
    f4 f8 f4 f8 |
    f4 f8 e4. |
    
    f4 f8 f4 f8 |
    f4 f8 e4 f8 |
    g4 e8 d4 f8 |
    f4 f8 e4 e8 |
    d4. c
  }
  \repeat volta 2{
    e4 e8 d4 d8 |
    e[ a] f e4. |
    e4 e8 f4 f8 |
    
    e4 g8 e4. |
    ees4 ees8 ees4 ees8 |
    d4 d8 d4 f8 |
    g4 e8 f4 f8 |
    
    %page2
    f8[ e] d e4 e8 |
    d4. c |
  }
  f4 f8 f4 f8 |
  f4 e8 e4 e8 |
  
  d4 c8 c4. |
  e d |
  c f |
  f4 e8 f4 f8 |
  
  f4 e8 f4. |
  f4 e8 f4 f8 |
  e4 d8 d4 d8 |
  c4 c8 a4. \bar"||"
  
  f'4 c8 f4 g8 |
  f4 g8 f4 f8 |
  a4 g8 f4 e8 |
  f4 f8 e4. |
  
  f4 e8 f4 f8 |
  f4 f8 f4 f8 |
  f4 e8 c4 \bar"||"
  c'8 |
  c4 a8 f4 f8 |
  
  %page3
  f4 f8 f4 c8 |
  f4 f8 e4 f8 |
  f4 e8 f4 f8 |
  f4 f8 f4 f8 |
  
  f4 e8 f4. \bar"||"
  f4 g8 f4 g8 |
  bes4 g8 f4 f8 |
  a4 g8 f4 e8 |
  
  f4 f8 e4. |
  f4 e8 f4 f8 |
  e4 f8 f4 f8 |
  f[ d] e f4. \bar"|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #" 1."
  Christ was born on Christ -- mas Day,
  Wreathe the hol -- ly, twine the bay;
  
  \markup\italic Chri -- \markup\italic stus \markup\italic na -- \markup\italic tus \markup\italic ho -- \markup\italic di -- \markup\italic "e :"
  The Babe, the Son, the Ho -- ly One of Ma -- ry.
  
  \set stanza = #" 3."
  Let the bright red ber -- ries glow
  Ev -- ’ry -- where in good -- ly show;
  
  \markup\italic Chri -- \markup\italic stus \markup\italic na -- \markup\italic tus \markup\italic ho -- \markup\italic di -- \markup\italic "e :"
  The Babe, the Son, the Ho -- ly One of Ma -- ry.
  
  \set stanza = #" 5."
  Night of sad -- ness: Morn of glad -- ness
  ev -- er -- more:
  Ev -- er, ev -- er:
  Af -- ter ma -- ny trou -- bles sore,
  Morn of glad -- ness ev -- er -- more and ev -- er -- more.
  
  \set stanza = #"   6."
  Mid -- night scarce -- ly pass’d and o -- ver,
  Draw -- ing to this ho -- ly morn,
  Ve -- ry ear -- ly, ve -- ry ear -- ly Christ was born.
  
  \set stanza = #"   7."
  Sing out with bliss, His Name is this:
  Em -- man -- u -- el:
  As was fore -- told in days of old
  By Ga -- bri -- el.
  
  \set stanza = #"   8."
  Mid -- night scarce -- ly pass’d and o -- ver,
  Draw -- ing to this ho -- ly morn,
  Ve -- ry ear -- ly, ve -- ry ear -- ly Christ was born.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #" 2."
  He is born to set us free,
  He is born our Lord to be,
  
  \markup\italic Ex \markup\italic Ma -- \markup\italic ri -- \markup\italic a \markup\italic Vir -- \markup\italic gi -- \markup\italic "ne :"
  The God, the Lord, by all a -- dor’d for ev -- er.
  
  \set stanza = #" 4."
  Chris -- tian men, re -- joice and sing;
  ’Tis the birth -- day of a King,
  
  \markup\italic Ex \markup\italic Ma -- \markup\italic ri -- \markup\italic a \markup\italic Vir -- \markup\italic gi -- \markup\italic "ne :"
  The God, the Lord, by all a -- dor’d for ev -- er.
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \repeat volta 2{
    a4 c8 a4 c8 |
    a4 a8 a4. |
    a4 c8 a4 c8 |
    a4 bes8 g4. |
    
    bes4 bes8 d4 c8 |
    d4 c8 c4 c8 |
    c4 c8 a4 d8 |
    c4 c8 c4 c8 |
    bes4. a
  }
  \repeat volta 2{
    g4 c8 a4 f8 |
    a4 a8 a4. |
    g4 c8 a4 d8 |
    
    c4 b8 g4. |
    g4 g8 g4 g8 |
    g4 a8 bes4 d8 |
    e4 c8 a4 d8 |
    
    %page2
    c4 b8 c4 c8 |
    bes[ f g] a4. |
  }
  a4 bes8 c4 d8 |
  c4 c8 c4 c8 |
  
  a[ bes] g a4. |
  g f |
  g c |
  c4 bes8 c4 d8 |
  
  c4 c8 c4. |
  c4 bes8 c4 f,8 |
  g4 g8 f4 f8 |
  f4 e8 f4. \bar"||"
  
  a4 g8 a4 c8 |
  c4 c8 a4 a8 |
  c4 c8 a4 a8 |
  f4 f8 g4. |
  
  c4 bes8 c4 d8 |
  c4 d8 c4 d8 |
  c4 c8 a4 \bar"||"
  c8 |
  c4 a8 f4 c'8 |
  
  %page3
  a4 a8 a4 f8 |
  a4 bes8 g4 f8 |
  a4 c8 c4 d8 |
  c4 d8 d4 d8 |
  
  c4 c8 a4. \bar"||"
  a4 g8 a4 c8 |
  d4 c8 a4 a8 |
  c4 c8 a4 a8 |
  
  f4 f8 g4. |
  c4 bes8 c4 bes8 |
  g4 bes8 c4 d8 |
  c4 c8 a4. \bar"|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat volta 2 {
    f4 f8 f4 f8 |
    f4 d8 f4. |
    f4 f8 f4 f8 |
    f4 bes,8 c4. |
    
    d4 d8 bes4 a8 |
    bes4 f'8 c4 f8 |
    e4 c8 d4 bes8 |
    f'4 c8 c4 a8 |
    bes4. f'\fermata
  }
  \repeat volta 2{
    c4 c8 d4 d8 |
    a'4 d,8 a4. |
    c4 c8 d4 d8 |
    
    a'4 g8 c,4. |
    ees4 ees8 ees4 c8 |
    g'4 g8 g4 d8 |
    c4 c8 d4 bes8 |
    
    %page2
    f'4 g8 c,4 a8 |
    bes4. f\fermata |
  }
  f'4 f8 f4 bes,8 |
  f'4 c8 a4 c8 |
  
  d[ bes] c f4. |
  c d |
  e f |
  a4 g8 f4 bes,8 |
  
  f'4 c8 f4. |
  a4 g8 f4 d8 |
  c4 g8 d'4 bes8 |
  c4 c8 f4.\fermata \bar"||"
  
  f4 e8 d4 c8 |
  a4 c8 d4 f8 |
  f4 c8 d4 a8 |
  d4 d8 c4. |
  
  a'4 g8 f4 d8 |
  a4 bes8 f'4 bes,8 |
  c4 c8 f4\fermata \bar"||"
  c'8 |
  c4 a8 f4 f8 |
  
  %page3
  f4 d8 f4\fermata f8 |
  f4 bes,8 c4\fermata f8 |
  d4 c8 f4 bes,8 |
  a4 bes8 d4 bes8 |
  
  c4 c8 f4.\fermata \bar"||"
  f4 e8 d4 c8 |
  bes4 c8 d4 f8 |
  f4 c8 d4 a8 |
  
  d4 d8 c4. |
  a'4 g8 f[ e] d |
  c4 d8 f4 bes,8 |
  c4 c8 f4.\fermata \bar"|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
  
  
  \midi {
    \tempo 4 = 95
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

