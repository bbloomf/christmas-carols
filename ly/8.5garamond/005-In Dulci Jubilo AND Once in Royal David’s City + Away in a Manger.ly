\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \concat{\italic "HymnsAndCarolsOfChristmas.com"}}
}
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
  markup-system-spacing =
   #'((basic-distance . 0)
      (minimum-distance . 0)
      (padding . 0)
      (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -20)
       (stretchability . 65))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #5
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }



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
  a4( g8) f4 \bar"" \break f8 |
  
  g4 g8 a4 g8 |
  f4( g8 a4) b8\rest |
  d,4 d8 e4 e8 |
  f4.( c'4) b8\rest |
  a4 a8 g4 g8 |
  f4. b4\rest \bar "|."
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
  \set stanza = "1. "
  \markup\italic In \markup\italic dul -- \markup\italic ci \markup\italic jú -- \markup\italic bi -- \markup\italic lo __ Now sing with hearts a -- glow! __
  Our heart’s joy re -- clin -- eth "" \markup\italic In \markup\italic præ -- \markup\italic sé -- \markup\italic pi -- \markup\italic o, __
  And like a bright star shin -- eth
  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O!
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = "2. "
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
  \set stanza = "3. "
  \markup\italic O \markup\italic Pa -- \markup\italic tris \markup\italic cá -- \markup\italic ri -- \markup\italic tas! __
  \markup\italic O \markup\italic na -- \markup\italic ti \markup\italic lé -- \markup\italic ni -- \markup\italic tas! __
  Deep -- ly were we stain -- ed
  \markup\italic Per \markup\italic nos -- \markup\italic tra \markup\italic crí -- \markup\italic mi -- \markup\italic na; __
  But Thou for us hast gain -- ed
  \markup\italic Cæ -- \markup\italic ló -- \markup\italic rum \markup\italic gáu -- \markup\italic di -- \markup\italic a. __
  \markup\italic Quan -- \markup\italic ta \markup\italic grá -- \markup\italic ti -- \markup\italic a! __
  \markup\italic Quan -- \markup\italic ta \markup\italic grá -- \markup\italic ti -- \markup\italic a!
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = "4. "
  \markup\italic U -- \markup\italic bi \markup\italic sunt \markup\italic gáu -- \markup\italic di -- \markup\italic a __
  In an -- y place but there? __
  There are an -- gels sing -- ing ""
  \markup\italic No -- \markup\italic va \markup\italic cán -- \markup\italic ti -- \markup\italic ca __
  And there the bells are ring -- ing
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  O that we were there! __
  O that we were there!
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In dulci jubilo"}}
    poet = \markup\oldStyleNum"Heinrich Seuse (1300–1366)"
    composer = \markup\oldStyleNum"14th century German melody"
    
    tagline = \markup { "from" \italic "ChristmasCarolMusic.org" }
  }
}





















global = {
  \key g \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \tempo 4 = 108
  \partial 2 d4 fis |
  g4. g8 g[ fis] g[ a] |
  a4 g g b |
  
  d4. b8 b[ a] g[ fis] |
  g2 \bar""\break d4 fis |
  g4. g8 g[ fis] g[ a] |
  
  a4 g g b |
  d4. b8 b[ a] g[ fis] |
  g2 \bar""\break e'4 e |
  
  d4. g,8 c4 c |
  \slurDashed b4( b) e e |
  \slurSolid d4. b8 b[ a] g[ fis] |
  \slurDashed g4( g) \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 c |
  d4. d8 d4 d8[ fis] |
  fis4 g d g |
  g4. g8 e4 d |
  d2 d4 c |
  
  d4. d8 cis4 cis |
  d8[ c!] b4 d g |
  g4. g8 e4 d |
  d2 g4 g |
  
  g8[ fis g] g g4 fis |
  \slurDashed g4( g) e8[ fis] g[ a] |
  d,[ fis g] d e4 d |
  d( d) \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = "1. "
  \set ignoreMelismata = ##t
  Once in roy -- al Da -- _ vid’s _ cit -- y
  Stood a low -- ly cat -- _ tle _ shed,
  Where a moth -- er laid _ her _ Ba -- by
  In a man -- ger for _ His _ bed:
  \set associatedVoice = "tenors"
  Ma -- ry was _ _ that moth -- er mild, _
  Je -- sus Christ _ _ her lit -- _ tle _ Child. _
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = "2. "
  \set ignoreMelismata = ##t
  He came down to earth _ from _ heav -- en,
  Who is God and Lord _ of _ all,
  And His shel -- ter was _ a _ sta -- ble,
  And His cra -- dle was _ a _ stall;
  \set associatedVoice = "tenors"
  With the poor, _ _ and mean, and low -- ly,
  Lived on earth _ _ our Sav -- _ ior _ ho -- ly.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = "3. "
  And, through all His won -- drous child -- hood,
    He would hon -- or and o -- bey,
  Love, and watch the low -- ly maid -- en
    In whose gen -- tle arms He lay;
  \set associatedVoice = "tenors"
  Chris -- tian chil -- dren all must be __
  Mild, o -- be -- dient, good as He. __
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = "4. "
  \set ignoreMelismata = ##t
  Je -- sus is our child -- _ hood’s _ pat -- tern,
  Day by day like us _ He _ grew;
  He was lit -- tle, weak, _ and _ help -- less,
  Tears and smiles, like us, _ He _ knew:
  \set associatedVoice = "tenors"
  And He feel -- _ _ eth for our sad -- ness,
  And He shar -- _ _ eth in _ our _ glad -- ness.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set ignoreMelismata = ##t
  \set stanza = "5. "
  And our eyes at last _ shall _ see Him,
  Through His own re -- deem -- _ ing _ love;
  For that Child so dear _ and _ gen -- tle
  Is our Lord in heav’n _ a -- _ bove:
  \set associatedVoice = "tenors"
  And He leads _ _ His chil -- dren on __ _
  To the place _ _ where He _ is _ gone. _
}

tenorMusic = \relative c' {
  b4 a |
  g4. b8 b[ a] b[ c] |
  c4 b b g |
  g4. d'8 d[ c] b[ a] |
  b2 b4 a |
  
  g4. b8 g4 g |
  fis g b g |
  g4. d'8 d[ c] b[ a] |
  b2 c4 c |
  
  b8[ c d] b a4 d |
  \slurDashed d4( d) c c |
  c8[ a b] d d[ c] b[ a] |
  b4( b) \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g4 a |
  b4. g8 d'4 d |
  d g g e |
  b4. g8 c4 d |
  g,2 g4 a |
  
  b4. g8 e'4 a, |
  d g g e |
  b4. g8 c4 d |
  g,2 c8[ d] e[ fis] |
  
  g[ a b] g d4 d |  
  \slurDashed g4( g) c,8[ d] e[ fis] |
  g4. g8 c,4 d |
  g,( g) \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "women" } \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Once in Royal David’s City"}}
    poet = \markup\oldStyleNum"Cecil Frances Alexander" % (1818–1895)"
    composer = \markup\oldStyleNum"Henry J. Gauntlett (1805–1876)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }
}
















\paper {
  scoreTitleMarkup = \markup {
    \fill-line {
      \fromproperty #'header:poet
      \fontsize #4 \bold \fromproperty #'header:title
      \fromproperty #'header:composer
    }
  }
}
global = {
  \key f \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \partial 4 c4 |
  c4. bes8 a4 |
  a g f |
  f e d |
  c2 c4 |
  c4. d8 c4 |
  
  c g' e |
  d c f |
  a2 c4 |
  c4. bes8 a4 |
  \slurDashed a g f |
  
  f e d |
  c2 c4 |
  bes'4. a8 g4 |
  a g f |
  g d e |
  f2 \bar "|."
%{IF_LESSER
\pageBreak
%}%END_IF_LESSER
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f4 |
  f4. g8 f4 |
  f e d |
  d c bes |
  a2 a4 |
  bes4. bes8 bes4 |
  
  bes bes bes |
  bes a c |
  f2 a8[ g] |
  f4. g8 f4 |
  f e d |
  
  d c bes |
  a2 c4 |
  e4. f8 g4 |
  f e d |
  d bes bes8[ c] |
  a2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIV
  \set stanza = "1. "
  A -- way in a man -- ger,
  No crib for His bed,
  The lit -- tle Lord Je -- sus
  Laid down His sweet head:
  The stars in the heav -- ens
  Look’d down where He lay,
  The lit -- tle Lord Je -- sus
  A -- sleep in the hay.
}
altoWordsII = \lyricmode {
  \dropLyricsIV
  \set stanza = "2. "
  The cat -- tle are low -- ing,
  The poor ba -- by wakes,
  But lit -- tle Lord Je -- sus
  No cry -- ing He makes;
  I love Thee, Lord Je -- sus,
  Look down from the sky,
  And stay by my cra -- dle
  Till mor -- ning is nigh.
}
altoWordsIII = \lyricmode {
  \dropLyricsIV
  \set stanza = "3. "
  Be near me, Lord Je -- sus,
  I ask Thee to stay
  Close by me for -- ev -- er
  And love me, I pray:
  Bless all the dear chil -- dren
  In Thy ten -- der care,
  And take us to heav -- en
  To live with Thee there.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c {
  a'4 |
  a4. bes8 c4 |
  c bes a |
  bes g f |
  f2 f4 |
  e4. f8 e4 |
  
  e e g |
  f f a |
  c2 c8[ bes] |
  a4. bes8 c4 |
  c bes a |
  
  bes g f |
  f2 c'4 |
  c4. c8 c4 |
  c bes a |
  bes g g |
  f2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4 |
  f4. e8 f4 |
  f,4 c' d |
  bes c bes8[ c] |
  f2 f4 |
  c4. bes8 c4 |
  
  c c c |
  bes f' f |
  f2 f4 |
  f4. e8 f4 |
  f, c' d |
  
  bes c bes8[ c] |
  f2 c4 |
  c4. d8 e4 |
  f c d |
  bes bes c4 |
  f,2 \bar "|."
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
    \new Lyrics \with { alignAboveContext = "women" } \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = "women" } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = "men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #2
  }
    %#(layout-set-staff-size 13)
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 17.8 20)))
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Away In A Manger"}}
    poet = \markup\oldStyleNum"Anonymous"
    composer = \markup\oldStyleNum"James Ramsey Murray (1841–1905)"
    tagline = ""%\markup { "from" \italic {ChristmasCarolMusic.org}}
  }
}
\paper {
  headerLine = ""
}