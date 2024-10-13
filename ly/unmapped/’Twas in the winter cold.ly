\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"’Twas in the winter cold"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "A Christmas Morning Hymn"}}
  poet = \markup\oldStyleNum"Rev. Charles I. Black (1821–1896)"
  composer = \markup\oldStyleNum"Joseph Barnby (1838–1896)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 2)
       (minimum-distance . 2)
       (padding . 1)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #127
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
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(0 . 0)
}

sopMusic = \relative c' {
  d4 d8 d e4 fis |
  fis4. e8 e4 fis |
  g^\< a b\! cis |
  a2~^\> a4\! \bar""\break a4 |
  
  d4. cis8 b4 fis |
  a4.^\markup\italic"dim." g8 g4 fis^\p |
  d4. d8 fis4 e |
  e2. \bar""\break e4 |
  
  g4. fis8 e4^\cresc e |
  a4. g8 fis4 fis |
  b4.-> a8 g4 fis\! |
  g2 \bar""\break a |
  
  b4. b8 cis4 cis |
  d^\f dis e b^\markup{\dynamic"p" \italic" rit."} |
  cis b g4. e8 |
  d1 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  a4 d8 d d4 d |
  d4. d8 d4 d |
  d d d cis |
  cis2( d4) e |
  
  d4. e8 fis4 d |
  fis4. e8 e4 cis |
  b4. d8 d4 d |
  cis2. e4 |
  
  d4. d8 d4 cis |
  d4. cis8 d4 e |
  dis4. dis8 dis4 dis |
  e2 fis |
  
  g4. d8 e4 e |
  d4 fis e e |
  e d b cis |
  d1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  ’Twas in the win -- ter cold, when earth Was de -- so -- late and wild, __
  That An -- gels wel -- comed at His Birth The ev -- er -- last -- ing Child.
  From realms of ev -- er bright -- ’ning day, And from His throne a -- bove
  He came, with hu -- man kind to stay, All low -- li -- ness and love.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Then in the man -- ger the poor beast
  Was pre -- sent with his Lord; __
  Then swains and pil -- grims from the East
  Saw, won -- dered, and a -- dored.
  And I this morn would come with them
  This bless -- ed sight to see,
  And to the Babe of Beth -- le -- hem
  Bend low the rev -- ’rent knee.
}
altoWordsIII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"mf " "3. "}
  \lyricmode {
    But I have not, it makes me sigh,
    One off -- ’ring in my pow’r;
  }
  \set stanza = \markup\dynamic"f "
  \lyricmode {
    ’Tis win -- ter all with me, and I
    Have nei -- ther fruit nor flow’r.
    O God, O Bro -- ther let me give,
    My worth -- less self to Thee;
    And that the years which I may live
    May pure and spot -- less be:
  }
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Grant me Thy -- self, O Sav -- ior kind,
  The Spi -- rit un -- de -- filed, __
  That I may be in heart and mind
  As gen -- tle as a child;
  That I may tread life’s ar -- duous ways
  As Thou Thy -- self hast trod,
  And in the might of prayer and praise
  Keep ev -- er close to God.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Light of the ev -- er -- last -- ing morn,
  Deep through my spi -- rit shine; __
  There let Thy pre  -- sence new -- ly born
  Make all my be -- ing Thine:
  There try me as the sil -- ver, try,
  And cleanse my soul with care,
  Till Thou art a -- ble to de -- scry
  Thy fault -- less im -- age there.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  fis4 fis8 fis a4 a |
  b4. b8 b4 a |
  b_\< a g\! g |
  g2(_\> fis4\!) a4 |
  
  fis4. fis8 fis4 b |
  b4. b8 b4 ais_\p |
  fis b a gis |
  a2. cis4 |
  
  b4. a8 g4 a |
  a4. a8 a4 a |
  a-> fis b a |
  g2 c |
  
  b4. b8 b4 ais |
  b_\f c b g_\p |
  g e e g |
  fis1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d4 d8 d d4 d |
  g,4. g8 g'4 fis |
  e e e a, |
  d2~ d4 cis |
  
  b4. cis8 d4 b |
  e4. e8 e4 fis |
  b,4. b8 e4 e |
  a,2. a'4 |
  
  a,4. a8 a4 g' |
  fis4. e8 d4 c |
  b4. b8 b4 b |
  e2 d |
  
  g4. g8 fis4 fis |
  b a g e |
  a, a a a |
  d1 \bar "|."
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \altoWordsII
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
  \midi {
    \tempo 4 = 80
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
