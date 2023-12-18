\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
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
       (padding . 0)
       (stretchability . 0))
  markup-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 40))
  
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #070
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
  \time 6/4
}

sopMusic = \relative c' {
  \partial 4 f4 |
  f2 f4 a2 bes4 |
  c2( d4 c2) \teeny c4 \normalsize |
  f,2 f4 a2 bes4 |
  c2( d4 c2) \teeny c4 \normalsize | \break

  c2 d4 c2 bes4 |
  a4.( bes8[ a g]) f2 \teeny f4 \normalsize |
  g2 g4 a2 g4 |
  f2( g4 a2) \teeny a4 \normalsize |
  c2 d4 c2 bes4 |
  a4.( bes8[ a g]) f2 f4 | \break

  g2 g4 a2 g4 |
  f2( g4 a2.) |
  d,2 d4 e2 e4 |
  f8([ e f g a bes] c2.) |
  a2 a4 g2 g4 |
  \partial 4*5 f2.~ f2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \partial 4 c4 |
  d2 d4 e2 e4 |
  f2.~ f2 \teeny f4 \normalsize |
  f( e) d4 c2 d4 |
  c( e g e2) \teeny e4 \normalsize |
  
  f2 f4 e( f) g |
  cis,( d e) f2 \teeny f4 \normalsize |
  f4( e8[ d]) e4 f2 e4 |
  f4.( g8[ f e] f2) \teeny f4 \normalsize |
  
  f2 f4 g( f) g |
  g( f e) d2 f4~ |
  f e8[ d] e4 f2 e4~ |
  e a,( d cis2.) |
  
  d2 d4~ d c8[ d] e4~ |
  e d2( c2.) |
  c4( f8[ e]) f4 f2 e4 |
  f8([ ees d c d bes] c2 \bar "|."

}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \markup\italic In \markup\italic dul -- \markup\italic ci \markup\italic jú -- \markup\italic bi -- \markup\italic lo __
  Nun sing -- et und seid froh! __ ""
  Al -- le un -- ser Won -- ne
  Liegt \markup\italic in \markup\italic præ -- \markup\italic sé -- \markup\italic pi -- \markup\italic o, __
  Sie leuch -- tet wie \set associatedVoice = "altos" 
  die Son --
  \unset associatedVoice
  ne
  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es 
  \set associatedVoice = "altos"
  \markup\italic et \markup\italic O! __

}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \markup\italic O \markup\italic Je -- \markup\italic su, \markup\italic pár -- \markup\italic vu -- \markup\italic le, __
  Nach Dir ist mir so weh. __ ""
  Tröst mir mein Ge -- mü -- te,
  \markup\italic O \markup\italic Pu -- \markup\italic er \markup\italic óp -- \markup\italic ti -- \markup\italic me, __ ""
  Durch all Dei -- \set associatedVoice = "altos" 
  ne Gü --
  \unset associatedVoice
  te,
  \markup\italic O \markup\italic Prin -- \markup\italic ceps \markup\italic gló -- \markup\italic ri -- \markup\italic æ. __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic Tra -- \markup\italic he \markup\italic me 
  \set associatedVoice = "altos"
  \markup\italic post \markup\italic Te! __
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \markup\italic O \markup\italic Pa -- \markup\italic tris \markup\italic cá -- \markup\italic ri -- \markup\italic tas! __
  \markup\italic O \markup\italic na -- \markup\italic ti \markup\italic lé -- \markup\italic ni -- \markup\italic tas! __
  Wir wär -- en all ver -- lor -- en,
  \markup\italic Per \markup\italic nos -- \markup\italic tra \markup\italic crí -- \markup\italic mi -- \markup\italic na; __
  So hat er uns \set associatedVoice = "altos" 
  er -- wor --
  \unset associatedVoice
  ben
  \markup\italic Cæ -- \markup\italic ló -- \markup\italic rum \markup\italic gáu -- \markup\italic di -- \markup\italic a. __
  \markup\italic Quan -- \markup\italic ta \markup\italic grá -- \markup\italic ti -- \markup\italic a! __
  \markup\italic Quan -- \markup\italic ta \markup\italic grá -- 
  \set associatedVoice = "altos"
  \markup\italic ti -- \markup\italic a! __
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \markup\italic U -- \markup\italic bi \markup\italic sunt \markup\italic gáu -- \markup\italic di -- \markup\italic a __ ""
  Nir -- gend mehr denn da, __ ""
  Wo die Eng -- el sing -- en ""
  \markup\italic No -- \markup\italic va \markup\italic cán -- \markup\italic ti -- \markup\italic ca __ ""
  Und die Har -- \set associatedVoice = "altos" 
  fen kling --
  \unset associatedVoice
  en
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  E -- ia, wär’n wir da! __
  E -- ia, wär’n
  \set associatedVoice = "altos"
  wir da! __
}

tenorMusic = \relative c' {
  \partial 4 a4 |
  a2 bes4 c2 bes4 |
  a2( bes4 a2) \teeny a4 \normalsize |
  a2 bes4 a( g) f |
  g( c b g2) \teeny g4 \normalsize |
  
  a2 bes4 c( d) e |
  a,( b cis) d2 \teeny c4 \normalsize |
  d( bes) c c2 c4 |
  c( a c c2) \teeny c4 \normalsize |
  
  a2 bes4 g2 e'4 |
  e( d cis) a2 c4 |
  d( bes) c c2 c4 |
  a2( bes4 e,2.) |
  
  r8 f[ g a] bes[ a] g4( a8[ bes]) c[ bes] |
  a4.( g8 f4 g2.) |
  a4( bes) c d( bes) c~ |
  c bes8([ a bes g] a2) \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \partial 4 f4 |
  d( c) bes a( bes8[ a]) g[ a] |
  f([ f' ees d c bes] f'2) \teeny f4 \normalsize |
  d( c) bes f'( e) d |
  e( c g c2) \teeny c4 \normalsize |
  
  f2 bes4~ bes a g~ |
  g f( e) d2 \teeny a4 \normalsize |
  bes( g) c f,8[ g a bes] c[ bes] |
  a4( f c f2) \teeny f4 \normalsize |
  
  f'4( e) d e( d) c |
  f8([ g] a4 a,) |
  d2 a4 |
  bes( g) c f,8[ g a bes] c[ a] |
  d4( c bes a2.) |
  
  bes4( a) g c( bes) a |
  d8([ c d e f d] e2.) |
  f,4( g) a bes( g) c |
  a( bes2 f2) \bar "|."
}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In dulci jubilo"}}
    poet = \markup\oldStyleNum"Heinrich Seuse (1300–1366)"
    composer = \markup\oldStyleNum"Arranged by J.S. Bach (1685–1750)"
    
    tagline = \markup \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }
}
\markup\fill-line{\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}}


















\header {
  tagline = \markup { "from" \italic "ChristmasCarolMusic.org" }
}
global = {
  \key f \major
  \time 6/8
}

sopMusic = \relative c' {
  \partial 8 f8 |
  f4 f8 a4 bes8 |
  c4( d8 c4) c8 |
  f,4 f8 a4 bes8 |
  c4( d8 c4) b8\rest |
  c4 d8 c4 bes8 |
  
  a4( g8) f4 \teeny f8 \normalsize |
  g4 g8 a4 g8 |
  f4( g8 a4) a8 |
  c4 d8 c4 bes8 |
  a4( g8) f4 f8 |
  
  g4 g8 a4 g8 |
  f4( g8 a4) b8\rest |
  d,4 d8 e4 e8 |
  f4.( c'4) b8\rest |
  a4 a8 g4 g8 |
  \partial 8*5 f4. b4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c8 |
  d4 c8 f[ e] d |
  c4( f8 e4) f8 |
  d4 c8 f[ e] d |
  c4( f8 e4) s8 |
  f4 f8 e4 g8 |
  
  f4( e8) f4 \teeny f8 \normalsize |
  f4 f8 e4 e8 |
  f4.~ f4 f8 |
  f4 f8 e4 g8 |
  f4( e8) f4 d8 |
  
  f4 f8 e4 e8 |
  f4( e8 f4) s8 |
  a,4 a8 d4 cis8 |
  d4.( e4) s8 |
  c4 f8 f4 e8 |
  f4. s4 \bar "|."

}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \markup\italic In \markup\italic dul -- \markup\italic ci \markup\italic jú -- \markup\italic bi -- \markup\italic lo __ Now sing with hearts a -- glow!
__
  Our de -- light and plea -- sure Lies \markup\italic in \markup\italic præ -- \markup\italic sé -- \markup\italic pi -- \markup\italic o, __
  Like sun -- shine is our trea -- sure
  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O!
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \markup\italic O \markup\italic Je -- \markup\italic su, \markup\italic pár -- \markup\italic vu -- \markup\italic le, __
  For thee I long al -- way; __
  Com -- fort my heart’s blind -- ness
  \markup\italic O \markup\italic Pu -- \markup\italic er \markup\italic óp -- \markup\italic ti -- \markup\italic me, __
  With all Thy lov -- ing kind -- ness,
  \markup\italic O \markup\italic Prin -- \markup\italic ceps \markup\italic gló -- \markup\italic ri -- \markup\italic æ. __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te!
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \markup\italic O \markup\italic Pa -- \markup\italic tris \markup\italic cá -- \markup\italic ri -- \markup\italic tas! __
  \markup\italic O \markup\italic na -- \markup\italic ti \markup\italic lé -- \markup\italic ni -- \markup\italic tas! __
  Deep -- ly were we stain -- ed
  \markup\italic Per \markup\italic nos -- \markup\italic tra \markup\italic crí -- \markup\italic mi -- \markup\italic na; __
  But Thou for us hast gain -- ed
  \markup\italic Cæ -- \markup\italic ló -- \markup\italic rum \markup\italic gáu -- \markup\italic di -- \markup\italic a. __
  O that we were there! O that we were there! 
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \markup\italic U -- \markup\italic bi \markup\italic sunt \markup\italic gáu -- \markup\italic di -- \markup\italic a __
  In an -- y place  but there?
  There are an -- gels sing -- ing ""
  \markup\italic No -- \markup\italic va \markup\italic cán -- \markup\italic ti -- \markup\italic ca __
  And there the bells are ring -- ing
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  O that we were there! O that we were there!
}

tenorMusic = \relative c' {
  a8 |
  bes4 a8 c4 bes8 |
  a4( bes8 g4) a8 |
  bes4 a8 c4 bes8 |
  a4( bes8 g4) s8 |
  c4 bes8 g4 c8 |
  
  c4. a4 \teeny a8 \normalsize |
  d4 d8 c4 bes8 |
  a4( bes8 c4) c8 |
  c4 bes8 g4 c8 |
  c4. a4 a8 |
  
  d4 d8 c[ d] bes |
  a4( c8)~ c4 s8 |
  a4 a8 g4 a8 |
  a4( bes8 g4) s8 |
  f4 a8 d4 c8 |
  a4. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8 |
  f4 f8 f4 f8 |
  f4.~ f4 f8 |
  f4 f8 f4 f8 |
  f4.( c4) d8\rest |
  a4 bes8 c4 e8 |
  
  f4( c8) d4 \teeny d8 \normalsize |
  bes4 bes8 c4 c8 |
  f4.~ f4 f8 |
  a,4 bes8 c4 e8 |
  f4( c8) d4 d8 |
  
  bes4 bes8 c4 c8 |
  d4( c8 f4) d8\rest |
  f4 f8 e4 a,8 |
  d4.( c4) d8\rest |
  f4 d8 bes4 c8 |
  f4. d4\rest \bar "|."

}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In dulci jubilo"}}
    poet = \markup\oldStyleNum"Heinrich Seuse (1300–1366)"
    composer = \markup\oldStyleNum"14th century German melody"
    
    tagline = \markup { "from" \italic "ChristmasCarolMusic.org" }
  }
}

