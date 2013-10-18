package cas

/*
 * RunParser.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

;

import scala.util.parsing.combinator._;
import scala.util.parsing.combinator.Parsers;

trait RunParser {
    this :RegexParsers =>
    type RootType;
    def root :Parser[RootType];
    def run(in :String) :ParseResult[RootType] = parseAll(root,in);
}
