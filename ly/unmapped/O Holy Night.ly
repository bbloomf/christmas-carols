\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"O Holy Night"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "(Cantique de Noël)"}}
  composer = \markup\oldStyleNum"Adolphe Adam (1803–1856)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  poet = \markup\oldStyleNum"Placide Cappeau (1808–1877)"
  meter = \markup\oldStyleNum"Translated by John Sullivan Dwight (1813–1893)"
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -15)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 0))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
%8.5x11  system-system-spacing #'stretchability = 100
%8.5x11  markup-system-spacing #'stretchability = 100
%8.5x11  top-system-spacing #'stretchability = 80
%8.5x11  last-bottom-spacing #'stretchability = 80
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #080
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
  \key des \major
  \time 4/4
  \autoBeamOff
  #'line-break-system-details #'((alignment-distances . (-100)))
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}
globalNoTime = {
  \key des \major
  \autoBeamOff
  #'line-break-system-details #'((alignment-distances . (-100)))
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusicPre = \relative c' {
  \tempo \markup\italic"Andante maestoso" 4 = 72
  r2 | r1 | r1 | r1 | \mark \markup { \musicglyph #"scripts.segno" }
  r2 f4^\mf f8.\noBeam f16 | \break
  
  \slurDotted aes4( aes8.\noBeam) aes16\noBeam bes8.\noBeam( bes16\noBeam) ges8.\noBeam bes16 |
  des2 aes8\noBeam r16 aes16 f8.\noBeam ees16 | \break
  
  des4 f8. ges16\noBeam aes4 ges8.\noBeam ees16 |
  des2. r4 |
  r2 f4 f8.\noBeam f16 | \pageBreak
  
  %page 2
  aes4( aes8.) aes16\noBeam bes8.\noBeam( bes16) ges8.\noBeam bes16 |
  des2 aes8\noBeam r16 aes16 g8.\noBeam f16\noBeam | \break
  
  c'4 aes8.\noBeam bes16 c4 \slurSolid \acciaccatura ees8 des8.\noBeam c16 |
  f,2 r4 r8 aes8\noBeam^\markup{\dynamic"p" \italic"piu mosso"} |
  aes4 bes ees, aes | \break
  
  bes8.\noBeam aes16 des8.\noBeam f,16 bes4 aes8.\noBeam aes16 |
  aes4^\< bes ees, aes |
  bes8.\noBeam\! aes16 des8. f,16 aes4~ aes8 r8 \pageBreak
}
sopMusic = \relative c'' {
  \repeat unfold 2 {
    des2. c8.\noBeam bes16 |
    c2. c4 |
    ees2( ees8.\noBeam) bes16\noBeam bes8.\noBeam bes16 |
    
    des2 des4 bes8\rest des8 |
    f2( ees4.) aes,8 |
    
    des2( des4)( c8.\noBeam) bes16 |
    aes2( aes8.\noBeam) aes16 bes8.\noBeam aes16 |
    aes2. des4 |
  }
  \alternative {
    %page 4
    %first ending
    {
      ees2. bes8\rest aes8 |
      f'2. ees4 |
      des2 c4 des8.\noBeam ees16 |
      
      des2. bes4\rest | \pageBreak
    }
    %second ending
    {
      ees2. ees4 |
    }
  }
  aes2~^\markup\italic"rit." aes8[ ges]^> f[^> ees]^> |
  des2 c4^\markup\italic"a tempo" des8.\noBeam ees16 |
  des2. bes4\rest\fermata \bar "|."
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark\markup\italic"D.S."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \repeat unfold 2 {
    f2. f8.\noBeam f16 |
    aes2. f4 |
    ges2( ges8.\noBeam) ges16 ges8.\noBeam ges16 |
    
    f2 f4 s8 f |
    aes2~ aes4. aes8 |
    
    aes2( bes4)( aes8.\noBeam) ges16 |
    f2( ges8.\noBeam) ges16 ges8.\noBeam ges16 |
    f2. aes4 |
  }
  
  %page 3
  \alternative {
    %first ending
    {
      aes2. s8 aes8 |
      aes2( bes4) ges4 |
      f2 ges4 ges8.\noBeam ges16 |
      
      f2. s4 |
    }
    %second ending
    {
      aes2. aes4 |
    }
  }
  aes2( bes4) bes |
  aes2 ges4 f8.\noBeam ges16 |
  f2. s4 \bar "|."
}
chorusWords = \lyricmode {
  \unset ignoreMelismata
  Fall on your knees, Oh, hear __ the an -- gel voi -- ces!
  O night __ di -- vine, __
  O night __ when Christ was born!
  \set associatedVoice = "altos"
  O night O ho -- ly night
  O night di -- vine!
  \unset associatedVoice
  
  \dropLyricsV
  Fall on your knees, Oh, hear __ the an -- gel \break voi -- ces!
  O night __ di -- vine, __
  O night __ when Christ was \break born!
  O night O ho -- ly __ night
  O night di -- vine!
}
chorusWordsII = {
  \set stanza = \markup\dynamic"f"
  \lyricmode {
    \unset ignoreMelismata
    He knows our need, Our weak -- ness is no stran -- ger.
    Be -- hold __ your King, __ be -- fore __ Him low -- ly bend!
    \set associatedVoice = "altos"
    Be -- hold your King, __ be -- fore Him low -- ly bend!
    \unset associatedVoice
  }
  \set stanza = \markup\dynamic"f"
  \lyricmode {
    \dropLyricsV
    He knows our need, Our weak -- ness is no stran -- ger.
    Be -- hold __ your King, __ be -- fore __ Him low -- ly bend!
    Be -- hold your King, __ be -- fore Him low -- ly bend!
  }
}
chorusWordsIII = \lyricmode {
  \unset ignoreMelismata
  Christ is the Lord, Oh, praise __ His name for -- ev -- er!
  His pow’r __ and glo -- ry ev -- er -- more pro -- claim!
  \set associatedVoice = "altos"
  His pow’r and glo -- ry ev -- er -- more pro -- claim!
  \unset associatedVoice
  
  \dropLyricsV
  Christ is the Lord, Oh, praise __ His name for -- ev -- er!
  His pow’r __ and glo -- ry ev -- er -- more pro -- claim!
  His pow’r and glo -- ry __ ev -- er -- more pro -- claim!
}

altoWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  O ho -- ly night __ _ the stars are bright -- ly shin -- ing,
  It is the night of the dear Sav -- ior’s birth;
  
  Long lay the world __ _ in sin and er -- ror pin -- ing,
  Till He ap -- peared and the soul felt its worth
  A thrill of hope the wea -- ry soul re -- joic -- es,
  For yon -- der breaks a new and glo -- rious morn; _
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Led by the light __ _ of faith se -- rene -- ly beam -- ing,
  With glow -- ing hearts by His cra -- dle we stand;
  
  So led by light of a star __ _ sweet -- ly gleam -- ing
  Here came the wise men from O -- ri -- ent land.
  The King of kings lay thus in low -- ly man -- ger,
  In all our tri -- als born to be our friend; _
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Tru -- ly He taught us to love __ _ one an -- oth -- er,
  His law is love and His gos -- pel is peace;
  
  Chains shall He break for the slave __ _ is our bro -- ther,
  And in His name all op -- pres -- sion shall cease.
  Sweet hymns of joy in grate -- ful cho -- rus raise we,
  Let all with -- in us praise His ho -- ly name; _
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  des2. ees8.\noBeam des16 |
  c2. aes4 |
  bes2( bes8.\noBeam) ees16 ees8.\noBeam ees16 |
  
  des2 des4 s8 des8 |
  des2( c4.) c8 |
  
  des2( des4)( des8.\noBeam) des16 |
  des2( c8.\noBeam) c16 c8.\noBeam c16 |
  des2. des4 |
  
  %page 3
  %first ending
%  c2. s8 c8 |
%  des2~ des4 bes |
%  aes2 ees'4 des8.\noBeam c16 |
  
%  des2. s4 |
  %second ending
  c2. c4 |
  
  des4( ces bes) ees |
  f2 ees4 des8.\noBeam c16 |
  aes2. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  bes2. bes8.\noBeam bes16 |
  f2. f4 |
  ees2~ ees8.\noBeam ees16 ees8.\noBeam ees16 |
  
  bes'2 bes4 d,8\rest bes'8 |
  aes2( ges4.) ges8 |
  
  f2( ges4)( ges8.\noBeam) ges16 |
  aes2( aes,8.) aes16\noBeam aes8. aes16\noBeam |
  des2. f4 |
  
  %page 4
  %first ending
%  aes2. des,8\rest ges8 |
%  f2( ges4) ges |
%  aes2 aes4 aes8.\noBeam aes,16 |
  
%  des2. des4\rest |
  %second ending
  aes2. ges4 |
  
  f2( ges4) ges |
  aes2 aes4 aes8.\noBeam aes16 |
  des,2. des4\rest\fermata \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c {
  \set Staff.midiInstrument = "acoustic grand"
  \override TupletBracket #'bracket-visibility = ##f 
  \mergeDifferentlyHeadedOn
%  \times 2/3 { f8[(_\mf des' aes} \times 2/3 { f' des aes])} \times 2/3 { f8[( des' aes } \times 2/3 { f' des aes]) } |
  
  \partial 2 <f' des>4_\mf q8.[ f16] |
  << {aes4~ aes8.[ aes16] bes4 ges8.[ bes16] |
      s1 |
      f''8[ des aes f] f4 ees!} \\
     {des,2_\< des |
      <e des'>2~\! q8[ <g bes e des'> <g bes e e'> q] |
      <aes des f>2 c |
     } >>
  
  \override TupletNumber #'text = #""
  \times 2/3 {
    f,,8[( des' aes f' des aes]) f8[( des' aes f' des aes]) |
    
    f8[( des' aes f' des aes]) ges[( des' bes ges' des bes]) |
    f8[( des' aes f' des aes]) f8[( des' aes f' des aes]) |
    
    f8[( des' aes f' des aes]) ges[( c aes ees' c aes]) |
    f8[( des' aes f' des aes]) f8[( des' aes f' des aes]) |
    f8[( des' aes f' des aes]) f8[( des' aes f' des aes]) |
    
    %page 2
    f8[( des' aes f' des aes]) ges[( des' bes ges' des bes]) |
    f8[( des' aes f' des aes]) f8[(_\> b aes f' b, aes]) |
    
    f8[(\! c' aes f' c aes]) g[( c bes e c bes]) |
    f8[(_\pp c' aes f' c aes]) f8[( c' aes f' c aes]) |
    ges8[( c aes ees' c aes]) ges8[( c aes ees' c aes]) |
    
    f8[( des' aes f' des aes]) f8[( des' aes f' des aes]) |
    ges8[(_\< c aes ees' c aes]) ges8[( c aes ees' c aes]) |
    f8[(\! des' aes f' des aes]) f8[( des' aes f' des aes]) |
  }
    %page 3 (Chorus)
    << \override TupletBracket #'bracket-visibility = ##f
       \override TupletNumber #'text = #"" \times 2/3 {
       \once\override DynamicText #'X-offset = #-6
      f8[_(_\f des' bes f' des bes]) f8[_( des' bes f' des bes]) |
      f8[_( c' aes f' c aes]) f8[_( c' aes f' c aes]) |
      ges[_( ees' bes ges' ees bes]) ges[_( ees' bes ges' ees bes]) |
      
      f8[_( des' bes f' des bes]) f8[_( des' bes f' des bes]) |
      f8[_( des' aes f' des aes]) ees8[_( c' aes ees' c aes]) |
      
      aes8[_( f' des aes' f des]) ges,8[_( des' bes ges' des bes]) |
      f8[_( des' aes f' des aes]) ges[_( c aes ees' c aes]) |
      f8[_( des' aes f' des aes]) f8[_( des' aes f' des aes]) |
    } \\ {
      f2 f | f f | ges ges |
      
      f f | f ees |
      
      aes ges | f ges | f f |      
    } >>
      
    %page 4
    %first ending
    << \override TupletBracket #'bracket-visibility = ##f
       \override TupletNumber #'text = #"" \times 2/3 {
      ees8[_( c' aes ees' c aes]) ges[_( c aes ees' c aes]) |
      aes8[_( f' des aes' f des]) ges,[_( ees' bes ges' ees bes]) |
      f8[_( des' aes f' des aes])
      } \\ {
      ees2 ges | aes ges | f
    } >>
    <ges aes c ees>4  r4 |
    << { \override TupletBracket #'bracket-visibility = ##f
       \override TupletNumber #'text = #"" \times 2/3 {
        f8[_( des' aes f' des aes]) f8[_( des'_\< aes des f aes]\!) |
      }} \\ { f,2 s} 
      >>
    
    << {       \once\override DynamicText #'X-offset = #-6
        des''8[_\f ees f des] bes[ des] c[ f16 bes,] |
        aes8[ c f g] aes[ g f c] |
        ees[ bes ees f] ges[ aes bes c] |
        des[ bes des ees] <des bes>4 <des, f,> | } \\
       {f,2. f4 |
        aes2.~ aes8[ aes] |
        ges2 bes4 ees |
        f2. s4 } >>
    
    \override TieColumn #'tie-configuration = #'((-2.0 . -1))
    <f aes,>2~^( <ees aes,>4.) aes,8 |
    \override TieColumn #'tie-configuration = #'()
    <aes des>2~_( <des bes>4) <c aes>8.[ <bes ges>16] |
    \override TieColumn #'tie-configuration = #'((0.0 . 1))
    <aes f>2~_( <aes ges c,>8.)[ q16] <bes ges c,>8.[ <aes ges c,>16] |
    <aes f des>2. <des aes f>4 |
    
    <ees aes, ges ees>2. 
    << { <aes, ees c>4 | <aes des aes'>2~ aes'8[ ges f ees] } \\
       { s4_\< | s8. s16\! ces4_\markup\italic"rit." bes4 <bes ges> } >>
    <des aes f>2 <c ges ees>4_\markup\italic"a tempo" <des aes f>8.[ <ees aes, ges>16] |
    <des aes f>2. r4 \bar "|."
}
pianoLH = \relative c {
  \set Staff.midiInstrument = "acoustic grand"
  \partial 2 <des aes'>2 |
  <f aes>2 <ges bes> |
  <g bes>2~ q4 s |
  s2 <aes ges'>2 |

  <des, aes des,>1 |
%  q |
  
  q2 <des ges,> |
  <des aes des,> <des f,> |
  
  <aes aes,>1 |
  <des aes des,> |
  q |
  
  %page 2
  q2 <des ges,> |
  <des aes des,> <des des,> |
  <c c,> <c c,> |
  f, r |
  << \new Voice { \voiceOne  c'4 aes c aes | des aes des aes | c aes c aes | des aes }
     \new Voice { \voiceTwo c,1 | des | c | des2 }
  >>
  <des des'>4 r |
  
  %page 3 (Chorus)
  <bes' bes,>1 |
  <f f,>1 |
  <ees ees,>1 |
  
  <bes' bes,> |
  <aes aes,>2 <ges ges,> |
  
  <f f,> <ges ges,> |
  <aes aes,> q |
  <des, des,>4-> <f f,>-> <aes aes,>-> <des des,>-> |
  
  %page 4
  % first ending
  <aes aes,>2 <ges ges,> |
  <f f,> <ges ges,> |
  <aes aes,> q4 r4 |
  
  <des aes des,>1 |
  
  <bes' des>2. << {ees8.[ des16]} \\ bes4 >> |
  <c f,>2.~ q8[ <aes f>] |
  << {bes2. ges4} \\ ees1 >> |
  <f bes,>2. <bes des>4 |
  
  <des aes>2^( <c ges>4.) q8 |
  <des f,>2~_( <des ges,>4) q |
  <des aes>2 aes, |
  <des des'>4 <c c'> <bes bes'> <aes aes'> |
  
  <c c'> <bes bes'> <aes aes'> <ges ges'> |
  <f f'>2 <ges ges'>4 q |
  <aes aes'>2 q4 <aes aes,>4 |
  <des des,>2. r4 \bar "|."
}

\score {
   <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos { s1 }
    \new Staff = women {
      \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
      \new Voice = "sopranos" {
        \global \sopMusicPre 
      }
      << \new Voice = "chorus" {
        \voiceOne \globalNoTime \sopMusic
      }
      \new Voice = "altos" {
        \voiceTwo \globalNoTime \altoMusic
      }
      >>
    }
    \new Lyrics = "altos" { s1 }
    \new Lyrics = "altosII" { s1 }
    \new Lyrics = "altosIII" { s1 }
    \new Lyrics = "altosIV" { s1 }
    \new Lyrics = "altosV" { s1 }
    \new Lyrics = "altosVI" { s1 }
 %   \new Lyrics = "tenors" { s1 }
    \new Staff = men <<
       \set Staff.explicitKeySignatureVisibility = #end-of-line-invisible
         \override Staff.TimeSignature #'stencil = ##f

      \set Staff.clefGlyph = #""
      \clef bass
      \new Voice = "tenors" {
        \voiceOne
        { s1*29 s2 \globalNoTime \tenorMusic }
      }
      \new Voice = "basses" {
        \voiceTwo { s1*29 s2 \globalNoTime \bassMusic }
      }
    >>
%    \new Lyrics = basses { s1 }
%    \context Lyrics = sopranos \lyricsto sopranos \sopWords
    \context Lyrics = altos \lyricsto sopranos \altoWords
    \context Lyrics = altosII \lyricsto sopranos \altoWordsII
    \context Lyrics = altosIII \lyricsto sopranos \altoWordsIII
    \context Lyrics = altos \lyricsto chorus \chorusWords 
    \context Lyrics = altosII \lyricsto chorus \chorusWordsII
    \context Lyrics = altosIII \lyricsto chorus \chorusWordsIII 
    \context Lyrics = altosIV \lyricsto sopranos \altoWordsIV
    \context Lyrics = altosV \lyricsto sopranos \altoWordsV
    \context Lyrics = altosVI \lyricsto sopranos \altoWordsVI
%    \context Lyrics = tenors \lyricsto tenors \tenorWords
%    \context Lyrics = basses \lyricsto basses \bassWords
   >>
    \new PianoStaff << \new Staff {
         
         \key des \major \time 4/4 \new Voice { \pianoRH } } \new Staff {
         
         \key des \major \time 4/4 \clef "bass" \pianoLH } >>
  >>
  \layout {
   % ragged-right = ##t
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 4)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
%      \override LyricText #'font-series = #'bold
    }
    \context {
      % a little smaller so lyrics
      % can be closer to the staff
      \Staff \RemoveEmptyStaves
      \override VerticalAxisGroup #'remove-first = ##t
      \override VerticalAxisGroup #'minimum-Y-extent = #'(-3 . 3)
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 5)
         (minimum-distance . 1)
         (padding . 1)
         (stretchability . 2))
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
