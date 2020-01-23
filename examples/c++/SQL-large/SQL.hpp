
#ifndef ___SQL__HPP__
#define ___SQL__HPP__

#include <memory>
#include <string>
#include <iostream>

namespace SQL {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// grammar definition

// syntax SQL (Query) {
//   QuerySelect : Query -> "select()" SelectModifier SelectExpressions FromClause WhereClause GroupByClause HavingClause OrderByClause LimitClause
//   AndConditionCondition : AndCondition -> Condition
//   AndConditionAnd : AndCondition -> Condition "and_()" AndCondition
//   AsEps : As -> eps
//   AsAs : As -> "as_()"
//   AscDescEps : AscDesc -> eps
//   AscDescAsc : AscDesc -> "asc()"
//   AscDescDesc : AscDesc -> "desc()"
//   BetweenConditionBetween : BetweenCondition -> "between()" Operand "and_()" Operand
//   BetweenConditionNotBetween : BetweenCondition -> "not_()" "between()" Operand "and_()" Operand
//   BindParameterQuestion : BindParameter -> "question()"
//   BindParameterRef : BindParameter -> "ref(int)"
//   Case : CaseClause -> "case_()" Term WhenClauses ElseClause "endCase()"
//   CaseWhen : CaseWhenClause -> "case_()" WhenClauses ElseClause "endCase()"
//   ColumnDefinition : ColumnDef -> ColumnRef DataType Nullability PrimaryKey
//   ColumnDefsLast : ColumnDefs -> ColumnDef
//   ColumnDefsCons : ColumnDefs -> ColumnDef "comma()" ColumnDefs
//   Column : ColumnRef -> FamilyName "column(std::string)"
//   CompareNE : Compare -> "ne()"
//   CompareLE : Compare -> "le()"
//   CompareGE : Compare -> "ge()"
//   CompareEQ : Compare -> "eq()"
//   CompareLT : Compare -> "lt()"
//   CompareGT : Compare -> "gt()"
//   ConditionCompare : Condition -> Operand Compare Operand
//   ConditionInCondition : Condition -> Operand InCondition
//   ConditionLikeCondition : Condition -> Operand LikeCondition
//   ConditionBetweenCondition : Condition -> Operand BetweenCondition
//   ConditionIsNullCondition : Condition -> Operand IsNullCondition
//   ConditionNotExpression : Condition -> "not_()" Expression
//   ConditionParen : Condition -> "lp()" Expression "rp()"
//   CharType : DataType -> "charType(int)"
//   IntegerType : DataType -> "integerType()"
//   BooleanType : DataType -> "booleanType()"
//   FloatType : DataType -> "floatType()"
//   DateType : DataType -> "dateType()"
//   ElseClauseEps : ElseClause -> eps
//   ElseClauseElse : ElseClause -> "else_()" Expression
//   ExpressionAndCondition : Expression -> AndCondition
//   ExpressionOr : Expression -> AndCondition "or_()" Expression
//   ExpressionsLast : Expressions -> Expression
//   ExpressionsCons : Expressions -> Expression "comma()" Expressions
//   FactorTerm : Factor -> Term
//   FactorTimes : Factor -> Factor "times()" Term
//   FactorDivides : Factor -> Factor "divides()" Term
//   FamilyNameEps : FamilyName -> eps
//   FamilyNameFamily : FamilyName -> "family(std::string)" "dot()"
//   From : FromClause -> "from_()" TableExpression
//   FromWithColumnDefs : FromClause -> "from_()" TableExpression ColumnDefs
//   GroupByClauseEps : GroupByClause -> eps
//   GroupByClauseGroupBy : GroupByClause -> "group()" "by()" Expressions
//   HavingClauseEps : HavingClause -> eps
//   HavingClauseHaving : HavingClause -> "having()" Expression
//   InConditionIn : InCondition -> "in_()" Operands
//   InConditionNotIn : InCondition -> "not_()" "in_()" Operands
//   IsNullConditionIsNull : IsNullCondition -> "is()" "null_()"
//   IsNullConditionIsNotNull : IsNullCondition -> "is()" "not_()" "null_()"
//   LikeConditionLike : LikeCondition -> "like()" Operand
//   LikeConditionNotLike : LikeCondition -> "not_()" "like()" Operand
//   LimitClauseEps : LimitClause -> eps
//   LimitClauseLimitBindParameter : LimitClause -> "limit()" BindParameter
//   LimitClauseLimitNumber : LimitClause -> "limit()" Number
//   NullabilityEps : Nullability -> eps
//   NullabilityNull : Nullability -> "null_()"
//   NullabilityNotNull : Nullability -> "not_()" "null_()"
//   NullsEps : Nulls -> eps
//   NullsFirst : Nulls -> "nulls()" "first_()"
//   NullsLast : Nulls -> "nulls()" "last_()"
//   NumberInteger : Number -> "integer(int)"
//   OperandSummand : Operand -> Summand
//   OperandAppend : Operand -> Operand "append()" Summand
//   OperandsLast : Operands -> Operand
//   OperandsCons : Operands -> Operand "comma()" Operands
//   OrderOrder : Order -> Expression AscDesc Nulls
//   OrderByClauseEps : OrderByClause -> eps
//   OrderByClauseOrderBy : OrderByClause -> "order()" "by()" Orders
//   OrdersLast : Orders -> Order
//   OrdersCons : Orders -> Order "comma()" Orders
//   PrimaryKeyEps : PrimaryKey -> eps
//   PrimaryKeyPrimaryKey : PrimaryKey -> "primary()" "key()" AscDesc
//   RowValue : RowValueConstructor -> "rawValue()" Term "comma()" Terms "endRawValue()"
//   SchemaNameEps : SchemaName -> eps
//   SchemaNameSchema : SchemaName -> "schema(std::string)" "dot()"
//   SelectExpressionAny : SelectExpression -> FamilyName "asterisk()"
//   SelectExpressionTerm : SelectExpression -> Term
//   SelectExpressionTermAs : SelectExpression -> Term As "alias(std::string)"
//   SelectExpressionsLast : SelectExpressions -> SelectExpression
//   SelectExpressionsCons : SelectExpressions -> SelectExpression "comma()" SelectExpressions
//   SelectModifierEps : SelectModifier -> eps
//   SelectModifierDistinct : SelectModifier -> "distinct()"
//   SelectModifierAll : SelectModifier -> "all_()"
//   SummandFactor : Summand -> Factor
//   SummandPlus : Summand -> Summand "plus()" Factor
//   SummandMinus : Summand -> Summand "minus()" Factor
//   TableAliasEps : TableAlias -> eps
//   TableAliasAlias : TableAlias -> As "alias(std::string)"
//   Table : TableExpression -> SchemaName "table(std::string)" TableAlias
//   TermValue : Term -> Value
//   TermBindParameter : Term -> BindParameter
//   TermCase : Term -> CaseClause
//   TermCaseWhen : Term -> CaseWhenClause
//   TermParen : Term -> "lp()" Operand "rp()"
//   TermColumnRef : Term -> ColumnRef
//   TermRowValueConstructor : Term -> RowValueConstructor
//   TermsLast : Terms -> Term
//   TermsCons : Terms -> Term "comma()" Terms
//   ValueString : Value -> "stringValue(std::string)"
//   ValueBoolean : Value -> "booleanValue(bool)"
//   ValueTrue : Value -> "true_()"
//   ValueFalse : Value -> "false_()"
//   ValueNumber : Value -> Number
//   ValueNull : Value -> "null_()"
//   When : WhenClause -> "when_()" Expression "then_()" Term
//   WhenClausesLast : WhenClauses -> WhenClause
//   WhenClausesCons : WhenClauses -> WhenClause WhenClauses
//   WhereClauseEps : WhereClause -> eps
//   WhereClauseWhere : WhereClause -> "where_()" Expression
// }

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node abstract classes

class Query {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Query() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class AndCondition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~AndCondition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class As {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~As() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class AscDesc {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~AscDesc() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class BetweenCondition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~BetweenCondition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class BindParameter {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~BindParameter() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class CaseClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~CaseClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class CaseWhenClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~CaseWhenClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class ColumnDef {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~ColumnDef() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class ColumnDefs {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~ColumnDefs() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class ColumnRef {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~ColumnRef() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Compare {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Compare() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Condition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Condition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class DataType {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~DataType() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class ElseClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~ElseClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Expression {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Expression() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Expressions {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Expressions() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Factor {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Factor() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class FamilyName {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~FamilyName() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class FromClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~FromClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class GroupByClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~GroupByClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class HavingClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~HavingClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class InCondition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~InCondition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class IsNullCondition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~IsNullCondition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class LikeCondition {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~LikeCondition() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class LimitClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~LimitClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Nullability {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Nullability() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Nulls {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Nulls() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Number {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Number() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Operand {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Operand() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Operands {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Operands() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Order {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Order() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class OrderByClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~OrderByClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Orders {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Orders() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class PrimaryKey {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~PrimaryKey() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class RowValueConstructor {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~RowValueConstructor() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class SchemaName {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~SchemaName() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class SelectExpression {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~SelectExpression() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class SelectExpressions {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~SelectExpressions() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class SelectModifier {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~SelectModifier() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Summand {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Summand() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class TableAlias {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~TableAlias() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class TableExpression {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~TableExpression() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Term {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Term() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Terms {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Terms() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class Value {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~Value() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class WhenClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~WhenClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class WhenClauses {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~WhenClauses() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

class WhereClause {
public:
  class Visitor;
  class ConstVisitor;

  virtual ~WhereClause() noexcept;

  virtual void accept( Visitor& ) = 0;
  virtual void accept( ConstVisitor& ) const = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// AST node concrete classes

class QuerySelect : public Query, public std::tuple< std::shared_ptr< SelectModifier >, std::shared_ptr< SelectExpressions >, std::shared_ptr< FromClause >, std::shared_ptr< WhereClause >, std::shared_ptr< GroupByClause >, std::shared_ptr< HavingClause >, std::shared_ptr< OrderByClause >, std::shared_ptr< LimitClause > > {
public:
  explicit QuerySelect( std::shared_ptr< SelectModifier > const& arg1, std::shared_ptr< SelectExpressions > const& arg2, std::shared_ptr< FromClause > const& arg3, std::shared_ptr< WhereClause > const& arg4, std::shared_ptr< GroupByClause > const& arg5, std::shared_ptr< HavingClause > const& arg6, std::shared_ptr< OrderByClause > const& arg7, std::shared_ptr< LimitClause > const& arg8 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class AndConditionCondition : public AndCondition, public std::tuple< std::shared_ptr< Condition > > {
public:
  explicit AndConditionCondition( std::shared_ptr< Condition > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class AndConditionAnd : public AndCondition, public std::tuple< std::shared_ptr< Condition >, std::shared_ptr< AndCondition > > {
public:
  explicit AndConditionAnd( std::shared_ptr< Condition > const& arg1, std::shared_ptr< AndCondition > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class AsEps : public As, public std::tuple<  > {
public:
  explicit AsEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class AsAs : public As, public std::tuple<  > {
public:
  explicit AsAs();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class AscDescEps : public AscDesc, public std::tuple<  > {
public:
  explicit AscDescEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class AscDescAsc : public AscDesc, public std::tuple<  > {
public:
  explicit AscDescAsc();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class AscDescDesc : public AscDesc, public std::tuple<  > {
public:
  explicit AscDescDesc();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class BetweenConditionBetween : public BetweenCondition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operand > > {
public:
  explicit BetweenConditionBetween( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operand > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class BetweenConditionNotBetween : public BetweenCondition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operand > > {
public:
  explicit BetweenConditionNotBetween( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operand > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class BindParameterQuestion : public BindParameter, public std::tuple<  > {
public:
  explicit BindParameterQuestion();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class BindParameterRef : public BindParameter, public std::tuple< int > {
public:
  explicit BindParameterRef( int const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Case : public CaseClause, public std::tuple< std::shared_ptr< Term >, std::shared_ptr< WhenClauses >, std::shared_ptr< ElseClause > > {
public:
  explicit Case( std::shared_ptr< Term > const& arg1, std::shared_ptr< WhenClauses > const& arg2, std::shared_ptr< ElseClause > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class CaseWhen : public CaseWhenClause, public std::tuple< std::shared_ptr< WhenClauses >, std::shared_ptr< ElseClause > > {
public:
  explicit CaseWhen( std::shared_ptr< WhenClauses > const& arg1, std::shared_ptr< ElseClause > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ColumnDefinition : public ColumnDef, public std::tuple< std::shared_ptr< ColumnRef >, std::shared_ptr< DataType >, std::shared_ptr< Nullability >, std::shared_ptr< PrimaryKey > > {
public:
  explicit ColumnDefinition( std::shared_ptr< ColumnRef > const& arg1, std::shared_ptr< DataType > const& arg2, std::shared_ptr< Nullability > const& arg3, std::shared_ptr< PrimaryKey > const& arg4 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ColumnDefsLast : public ColumnDefs, public std::tuple< std::shared_ptr< ColumnDef > > {
public:
  explicit ColumnDefsLast( std::shared_ptr< ColumnDef > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ColumnDefsCons : public ColumnDefs, public std::tuple< std::shared_ptr< ColumnDef >, std::shared_ptr< ColumnDefs > > {
public:
  explicit ColumnDefsCons( std::shared_ptr< ColumnDef > const& arg1, std::shared_ptr< ColumnDefs > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Column : public ColumnRef, public std::tuple< std::shared_ptr< FamilyName >, std::string > {
public:
  explicit Column( std::shared_ptr< FamilyName > const& arg1, std::string const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class CompareNE : public Compare, public std::tuple<  > {
public:
  explicit CompareNE();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class CompareLE : public Compare, public std::tuple<  > {
public:
  explicit CompareLE();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class CompareGE : public Compare, public std::tuple<  > {
public:
  explicit CompareGE();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class CompareEQ : public Compare, public std::tuple<  > {
public:
  explicit CompareEQ();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class CompareLT : public Compare, public std::tuple<  > {
public:
  explicit CompareLT();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class CompareGT : public Compare, public std::tuple<  > {
public:
  explicit CompareGT();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ConditionCompare : public Condition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Compare >, std::shared_ptr< Operand > > {
public:
  explicit ConditionCompare( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Compare > const& arg2, std::shared_ptr< Operand > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionInCondition : public Condition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< InCondition > > {
public:
  explicit ConditionInCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< InCondition > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionLikeCondition : public Condition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< LikeCondition > > {
public:
  explicit ConditionLikeCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< LikeCondition > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionBetweenCondition : public Condition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< BetweenCondition > > {
public:
  explicit ConditionBetweenCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< BetweenCondition > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionIsNullCondition : public Condition, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< IsNullCondition > > {
public:
  explicit ConditionIsNullCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< IsNullCondition > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionNotExpression : public Condition, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit ConditionNotExpression( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ConditionParen : public Condition, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit ConditionParen( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class CharType : public DataType, public std::tuple< int > {
public:
  explicit CharType( int const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class IntegerType : public DataType, public std::tuple<  > {
public:
  explicit IntegerType();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class BooleanType : public DataType, public std::tuple<  > {
public:
  explicit BooleanType();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FloatType : public DataType, public std::tuple<  > {
public:
  explicit FloatType();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class DateType : public DataType, public std::tuple<  > {
public:
  explicit DateType();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ElseClauseEps : public ElseClause, public std::tuple<  > {
public:
  explicit ElseClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ElseClauseElse : public ElseClause, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit ElseClauseElse( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ExpressionAndCondition : public Expression, public std::tuple< std::shared_ptr< AndCondition > > {
public:
  explicit ExpressionAndCondition( std::shared_ptr< AndCondition > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ExpressionOr : public Expression, public std::tuple< std::shared_ptr< AndCondition >, std::shared_ptr< Expression > > {
public:
  explicit ExpressionOr( std::shared_ptr< AndCondition > const& arg1, std::shared_ptr< Expression > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ExpressionsLast : public Expressions, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit ExpressionsLast( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ExpressionsCons : public Expressions, public std::tuple< std::shared_ptr< Expression >, std::shared_ptr< Expressions > > {
public:
  explicit ExpressionsCons( std::shared_ptr< Expression > const& arg1, std::shared_ptr< Expressions > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class FactorTerm : public Factor, public std::tuple< std::shared_ptr< Term > > {
public:
  explicit FactorTerm( std::shared_ptr< Term > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FactorTimes : public Factor, public std::tuple< std::shared_ptr< Factor >, std::shared_ptr< Term > > {
public:
  explicit FactorTimes( std::shared_ptr< Factor > const& arg1, std::shared_ptr< Term > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FactorDivides : public Factor, public std::tuple< std::shared_ptr< Factor >, std::shared_ptr< Term > > {
public:
  explicit FactorDivides( std::shared_ptr< Factor > const& arg1, std::shared_ptr< Term > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class FamilyNameEps : public FamilyName, public std::tuple<  > {
public:
  explicit FamilyNameEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FamilyNameFamily : public FamilyName, public std::tuple< std::string > {
public:
  explicit FamilyNameFamily( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class From : public FromClause, public std::tuple< std::shared_ptr< TableExpression > > {
public:
  explicit From( std::shared_ptr< TableExpression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class FromWithColumnDefs : public FromClause, public std::tuple< std::shared_ptr< TableExpression >, std::shared_ptr< ColumnDefs > > {
public:
  explicit FromWithColumnDefs( std::shared_ptr< TableExpression > const& arg1, std::shared_ptr< ColumnDefs > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class GroupByClauseEps : public GroupByClause, public std::tuple<  > {
public:
  explicit GroupByClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class GroupByClauseGroupBy : public GroupByClause, public std::tuple< std::shared_ptr< Expressions > > {
public:
  explicit GroupByClauseGroupBy( std::shared_ptr< Expressions > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class HavingClauseEps : public HavingClause, public std::tuple<  > {
public:
  explicit HavingClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class HavingClauseHaving : public HavingClause, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit HavingClauseHaving( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class InConditionIn : public InCondition, public std::tuple< std::shared_ptr< Operands > > {
public:
  explicit InConditionIn( std::shared_ptr< Operands > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class InConditionNotIn : public InCondition, public std::tuple< std::shared_ptr< Operands > > {
public:
  explicit InConditionNotIn( std::shared_ptr< Operands > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class IsNullConditionIsNull : public IsNullCondition, public std::tuple<  > {
public:
  explicit IsNullConditionIsNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class IsNullConditionIsNotNull : public IsNullCondition, public std::tuple<  > {
public:
  explicit IsNullConditionIsNotNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class LikeConditionLike : public LikeCondition, public std::tuple< std::shared_ptr< Operand > > {
public:
  explicit LikeConditionLike( std::shared_ptr< Operand > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class LikeConditionNotLike : public LikeCondition, public std::tuple< std::shared_ptr< Operand > > {
public:
  explicit LikeConditionNotLike( std::shared_ptr< Operand > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class LimitClauseEps : public LimitClause, public std::tuple<  > {
public:
  explicit LimitClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class LimitClauseLimitBindParameter : public LimitClause, public std::tuple< std::shared_ptr< BindParameter > > {
public:
  explicit LimitClauseLimitBindParameter( std::shared_ptr< BindParameter > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class LimitClauseLimitNumber : public LimitClause, public std::tuple< std::shared_ptr< Number > > {
public:
  explicit LimitClauseLimitNumber( std::shared_ptr< Number > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NullabilityEps : public Nullability, public std::tuple<  > {
public:
  explicit NullabilityEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NullabilityNull : public Nullability, public std::tuple<  > {
public:
  explicit NullabilityNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NullabilityNotNull : public Nullability, public std::tuple<  > {
public:
  explicit NullabilityNotNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NullsEps : public Nulls, public std::tuple<  > {
public:
  explicit NullsEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NullsFirst : public Nulls, public std::tuple<  > {
public:
  explicit NullsFirst();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class NullsLast : public Nulls, public std::tuple<  > {
public:
  explicit NullsLast();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class NumberInteger : public Number, public std::tuple< int > {
public:
  explicit NumberInteger( int const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class OperandSummand : public Operand, public std::tuple< std::shared_ptr< Summand > > {
public:
  explicit OperandSummand( std::shared_ptr< Summand > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class OperandAppend : public Operand, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Summand > > {
public:
  explicit OperandAppend( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Summand > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class OperandsLast : public Operands, public std::tuple< std::shared_ptr< Operand > > {
public:
  explicit OperandsLast( std::shared_ptr< Operand > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class OperandsCons : public Operands, public std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operands > > {
public:
  explicit OperandsCons( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operands > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class OrderOrder : public Order, public std::tuple< std::shared_ptr< Expression >, std::shared_ptr< AscDesc >, std::shared_ptr< Nulls > > {
public:
  explicit OrderOrder( std::shared_ptr< Expression > const& arg1, std::shared_ptr< AscDesc > const& arg2, std::shared_ptr< Nulls > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class OrderByClauseEps : public OrderByClause, public std::tuple<  > {
public:
  explicit OrderByClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class OrderByClauseOrderBy : public OrderByClause, public std::tuple< std::shared_ptr< Orders > > {
public:
  explicit OrderByClauseOrderBy( std::shared_ptr< Orders > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class OrdersLast : public Orders, public std::tuple< std::shared_ptr< Order > > {
public:
  explicit OrdersLast( std::shared_ptr< Order > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class OrdersCons : public Orders, public std::tuple< std::shared_ptr< Order >, std::shared_ptr< Orders > > {
public:
  explicit OrdersCons( std::shared_ptr< Order > const& arg1, std::shared_ptr< Orders > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class PrimaryKeyEps : public PrimaryKey, public std::tuple<  > {
public:
  explicit PrimaryKeyEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class PrimaryKeyPrimaryKey : public PrimaryKey, public std::tuple< std::shared_ptr< AscDesc > > {
public:
  explicit PrimaryKeyPrimaryKey( std::shared_ptr< AscDesc > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class RowValue : public RowValueConstructor, public std::tuple< std::shared_ptr< Term >, std::shared_ptr< Terms > > {
public:
  explicit RowValue( std::shared_ptr< Term > const& arg1, std::shared_ptr< Terms > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SchemaNameEps : public SchemaName, public std::tuple<  > {
public:
  explicit SchemaNameEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SchemaNameSchema : public SchemaName, public std::tuple< std::string > {
public:
  explicit SchemaNameSchema( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SelectExpressionAny : public SelectExpression, public std::tuple< std::shared_ptr< FamilyName > > {
public:
  explicit SelectExpressionAny( std::shared_ptr< FamilyName > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SelectExpressionTerm : public SelectExpression, public std::tuple< std::shared_ptr< Term > > {
public:
  explicit SelectExpressionTerm( std::shared_ptr< Term > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SelectExpressionTermAs : public SelectExpression, public std::tuple< std::shared_ptr< Term >, std::shared_ptr< As >, std::string > {
public:
  explicit SelectExpressionTermAs( std::shared_ptr< Term > const& arg1, std::shared_ptr< As > const& arg2, std::string const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SelectExpressionsLast : public SelectExpressions, public std::tuple< std::shared_ptr< SelectExpression > > {
public:
  explicit SelectExpressionsLast( std::shared_ptr< SelectExpression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SelectExpressionsCons : public SelectExpressions, public std::tuple< std::shared_ptr< SelectExpression >, std::shared_ptr< SelectExpressions > > {
public:
  explicit SelectExpressionsCons( std::shared_ptr< SelectExpression > const& arg1, std::shared_ptr< SelectExpressions > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SelectModifierEps : public SelectModifier, public std::tuple<  > {
public:
  explicit SelectModifierEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SelectModifierDistinct : public SelectModifier, public std::tuple<  > {
public:
  explicit SelectModifierDistinct();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SelectModifierAll : public SelectModifier, public std::tuple<  > {
public:
  explicit SelectModifierAll();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class SummandFactor : public Summand, public std::tuple< std::shared_ptr< Factor > > {
public:
  explicit SummandFactor( std::shared_ptr< Factor > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SummandPlus : public Summand, public std::tuple< std::shared_ptr< Summand >, std::shared_ptr< Factor > > {
public:
  explicit SummandPlus( std::shared_ptr< Summand > const& arg1, std::shared_ptr< Factor > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class SummandMinus : public Summand, public std::tuple< std::shared_ptr< Summand >, std::shared_ptr< Factor > > {
public:
  explicit SummandMinus( std::shared_ptr< Summand > const& arg1, std::shared_ptr< Factor > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class TableAliasEps : public TableAlias, public std::tuple<  > {
public:
  explicit TableAliasEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TableAliasAlias : public TableAlias, public std::tuple< std::shared_ptr< As >, std::string > {
public:
  explicit TableAliasAlias( std::shared_ptr< As > const& arg1, std::string const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class Table : public TableExpression, public std::tuple< std::shared_ptr< SchemaName >, std::string, std::shared_ptr< TableAlias > > {
public:
  explicit Table( std::shared_ptr< SchemaName > const& arg1, std::string const& arg2, std::shared_ptr< TableAlias > const& arg3 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class TermValue : public Term, public std::tuple< std::shared_ptr< Value > > {
public:
  explicit TermValue( std::shared_ptr< Value > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermBindParameter : public Term, public std::tuple< std::shared_ptr< BindParameter > > {
public:
  explicit TermBindParameter( std::shared_ptr< BindParameter > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermCase : public Term, public std::tuple< std::shared_ptr< CaseClause > > {
public:
  explicit TermCase( std::shared_ptr< CaseClause > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermCaseWhen : public Term, public std::tuple< std::shared_ptr< CaseWhenClause > > {
public:
  explicit TermCaseWhen( std::shared_ptr< CaseWhenClause > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermParen : public Term, public std::tuple< std::shared_ptr< Operand > > {
public:
  explicit TermParen( std::shared_ptr< Operand > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermColumnRef : public Term, public std::tuple< std::shared_ptr< ColumnRef > > {
public:
  explicit TermColumnRef( std::shared_ptr< ColumnRef > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermRowValueConstructor : public Term, public std::tuple< std::shared_ptr< RowValueConstructor > > {
public:
  explicit TermRowValueConstructor( std::shared_ptr< RowValueConstructor > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class TermsLast : public Terms, public std::tuple< std::shared_ptr< Term > > {
public:
  explicit TermsLast( std::shared_ptr< Term > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class TermsCons : public Terms, public std::tuple< std::shared_ptr< Term >, std::shared_ptr< Terms > > {
public:
  explicit TermsCons( std::shared_ptr< Term > const& arg1, std::shared_ptr< Terms > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class ValueString : public Value, public std::tuple< std::string > {
public:
  explicit ValueString( std::string const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ValueBoolean : public Value, public std::tuple< bool > {
public:
  explicit ValueBoolean( bool const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ValueTrue : public Value, public std::tuple<  > {
public:
  explicit ValueTrue();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ValueFalse : public Value, public std::tuple<  > {
public:
  explicit ValueFalse();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ValueNumber : public Value, public std::tuple< std::shared_ptr< Number > > {
public:
  explicit ValueNumber( std::shared_ptr< Number > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class ValueNull : public Value, public std::tuple<  > {
public:
  explicit ValueNull();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class When : public WhenClause, public std::tuple< std::shared_ptr< Expression >, std::shared_ptr< Term > > {
public:
  explicit When( std::shared_ptr< Expression > const& arg1, std::shared_ptr< Term > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class WhenClausesLast : public WhenClauses, public std::tuple< std::shared_ptr< WhenClause > > {
public:
  explicit WhenClausesLast( std::shared_ptr< WhenClause > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class WhenClausesCons : public WhenClauses, public std::tuple< std::shared_ptr< WhenClause >, std::shared_ptr< WhenClauses > > {
public:
  explicit WhenClausesCons( std::shared_ptr< WhenClause > const& arg1, std::shared_ptr< WhenClauses > const& arg2 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};


class WhereClauseEps : public WhereClause, public std::tuple<  > {
public:
  explicit WhereClauseEps();

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

class WhereClauseWhere : public WhereClause, public std::tuple< std::shared_ptr< Expression > > {
public:
  explicit WhereClauseWhere( std::shared_ptr< Expression > const& arg1 );

  void accept( Visitor& visitor );
  void accept( ConstVisitor& visitor ) const;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// visitors

class Query::Visitor{
public:
  virtual void visitQuerySelect( QuerySelect& host ) = 0;
};

class Query::ConstVisitor{
public:
  virtual void visitQuerySelect( QuerySelect const& host ) = 0;
};

class AndCondition::Visitor{
public:
  virtual void visitAndConditionCondition( AndConditionCondition& host ) = 0;
  virtual void visitAndConditionAnd( AndConditionAnd& host ) = 0;
};

class AndCondition::ConstVisitor{
public:
  virtual void visitAndConditionCondition( AndConditionCondition const& host ) = 0;
  virtual void visitAndConditionAnd( AndConditionAnd const& host ) = 0;
};

class As::Visitor{
public:
  virtual void visitAsEps( AsEps& host ) = 0;
  virtual void visitAsAs( AsAs& host ) = 0;
};

class As::ConstVisitor{
public:
  virtual void visitAsEps( AsEps const& host ) = 0;
  virtual void visitAsAs( AsAs const& host ) = 0;
};

class AscDesc::Visitor{
public:
  virtual void visitAscDescEps( AscDescEps& host ) = 0;
  virtual void visitAscDescAsc( AscDescAsc& host ) = 0;
  virtual void visitAscDescDesc( AscDescDesc& host ) = 0;
};

class AscDesc::ConstVisitor{
public:
  virtual void visitAscDescEps( AscDescEps const& host ) = 0;
  virtual void visitAscDescAsc( AscDescAsc const& host ) = 0;
  virtual void visitAscDescDesc( AscDescDesc const& host ) = 0;
};

class BetweenCondition::Visitor{
public:
  virtual void visitBetweenConditionBetween( BetweenConditionBetween& host ) = 0;
  virtual void visitBetweenConditionNotBetween( BetweenConditionNotBetween& host ) = 0;
};

class BetweenCondition::ConstVisitor{
public:
  virtual void visitBetweenConditionBetween( BetweenConditionBetween const& host ) = 0;
  virtual void visitBetweenConditionNotBetween( BetweenConditionNotBetween const& host ) = 0;
};

class BindParameter::Visitor{
public:
  virtual void visitBindParameterQuestion( BindParameterQuestion& host ) = 0;
  virtual void visitBindParameterRef( BindParameterRef& host ) = 0;
};

class BindParameter::ConstVisitor{
public:
  virtual void visitBindParameterQuestion( BindParameterQuestion const& host ) = 0;
  virtual void visitBindParameterRef( BindParameterRef const& host ) = 0;
};

class CaseClause::Visitor{
public:
  virtual void visitCase( Case& host ) = 0;
};

class CaseClause::ConstVisitor{
public:
  virtual void visitCase( Case const& host ) = 0;
};

class CaseWhenClause::Visitor{
public:
  virtual void visitCaseWhen( CaseWhen& host ) = 0;
};

class CaseWhenClause::ConstVisitor{
public:
  virtual void visitCaseWhen( CaseWhen const& host ) = 0;
};

class ColumnDef::Visitor{
public:
  virtual void visitColumnDefinition( ColumnDefinition& host ) = 0;
};

class ColumnDef::ConstVisitor{
public:
  virtual void visitColumnDefinition( ColumnDefinition const& host ) = 0;
};

class ColumnDefs::Visitor{
public:
  virtual void visitColumnDefsLast( ColumnDefsLast& host ) = 0;
  virtual void visitColumnDefsCons( ColumnDefsCons& host ) = 0;
};

class ColumnDefs::ConstVisitor{
public:
  virtual void visitColumnDefsLast( ColumnDefsLast const& host ) = 0;
  virtual void visitColumnDefsCons( ColumnDefsCons const& host ) = 0;
};

class ColumnRef::Visitor{
public:
  virtual void visitColumn( Column& host ) = 0;
};

class ColumnRef::ConstVisitor{
public:
  virtual void visitColumn( Column const& host ) = 0;
};

class Compare::Visitor{
public:
  virtual void visitCompareNE( CompareNE& host ) = 0;
  virtual void visitCompareLE( CompareLE& host ) = 0;
  virtual void visitCompareGE( CompareGE& host ) = 0;
  virtual void visitCompareEQ( CompareEQ& host ) = 0;
  virtual void visitCompareLT( CompareLT& host ) = 0;
  virtual void visitCompareGT( CompareGT& host ) = 0;
};

class Compare::ConstVisitor{
public:
  virtual void visitCompareNE( CompareNE const& host ) = 0;
  virtual void visitCompareLE( CompareLE const& host ) = 0;
  virtual void visitCompareGE( CompareGE const& host ) = 0;
  virtual void visitCompareEQ( CompareEQ const& host ) = 0;
  virtual void visitCompareLT( CompareLT const& host ) = 0;
  virtual void visitCompareGT( CompareGT const& host ) = 0;
};

class Condition::Visitor{
public:
  virtual void visitConditionCompare( ConditionCompare& host ) = 0;
  virtual void visitConditionInCondition( ConditionInCondition& host ) = 0;
  virtual void visitConditionLikeCondition( ConditionLikeCondition& host ) = 0;
  virtual void visitConditionBetweenCondition( ConditionBetweenCondition& host ) = 0;
  virtual void visitConditionIsNullCondition( ConditionIsNullCondition& host ) = 0;
  virtual void visitConditionNotExpression( ConditionNotExpression& host ) = 0;
  virtual void visitConditionParen( ConditionParen& host ) = 0;
};

class Condition::ConstVisitor{
public:
  virtual void visitConditionCompare( ConditionCompare const& host ) = 0;
  virtual void visitConditionInCondition( ConditionInCondition const& host ) = 0;
  virtual void visitConditionLikeCondition( ConditionLikeCondition const& host ) = 0;
  virtual void visitConditionBetweenCondition( ConditionBetweenCondition const& host ) = 0;
  virtual void visitConditionIsNullCondition( ConditionIsNullCondition const& host ) = 0;
  virtual void visitConditionNotExpression( ConditionNotExpression const& host ) = 0;
  virtual void visitConditionParen( ConditionParen const& host ) = 0;
};

class DataType::Visitor{
public:
  virtual void visitCharType( CharType& host ) = 0;
  virtual void visitIntegerType( IntegerType& host ) = 0;
  virtual void visitBooleanType( BooleanType& host ) = 0;
  virtual void visitFloatType( FloatType& host ) = 0;
  virtual void visitDateType( DateType& host ) = 0;
};

class DataType::ConstVisitor{
public:
  virtual void visitCharType( CharType const& host ) = 0;
  virtual void visitIntegerType( IntegerType const& host ) = 0;
  virtual void visitBooleanType( BooleanType const& host ) = 0;
  virtual void visitFloatType( FloatType const& host ) = 0;
  virtual void visitDateType( DateType const& host ) = 0;
};

class ElseClause::Visitor{
public:
  virtual void visitElseClauseEps( ElseClauseEps& host ) = 0;
  virtual void visitElseClauseElse( ElseClauseElse& host ) = 0;
};

class ElseClause::ConstVisitor{
public:
  virtual void visitElseClauseEps( ElseClauseEps const& host ) = 0;
  virtual void visitElseClauseElse( ElseClauseElse const& host ) = 0;
};

class Expression::Visitor{
public:
  virtual void visitExpressionAndCondition( ExpressionAndCondition& host ) = 0;
  virtual void visitExpressionOr( ExpressionOr& host ) = 0;
};

class Expression::ConstVisitor{
public:
  virtual void visitExpressionAndCondition( ExpressionAndCondition const& host ) = 0;
  virtual void visitExpressionOr( ExpressionOr const& host ) = 0;
};

class Expressions::Visitor{
public:
  virtual void visitExpressionsLast( ExpressionsLast& host ) = 0;
  virtual void visitExpressionsCons( ExpressionsCons& host ) = 0;
};

class Expressions::ConstVisitor{
public:
  virtual void visitExpressionsLast( ExpressionsLast const& host ) = 0;
  virtual void visitExpressionsCons( ExpressionsCons const& host ) = 0;
};

class Factor::Visitor{
public:
  virtual void visitFactorTerm( FactorTerm& host ) = 0;
  virtual void visitFactorTimes( FactorTimes& host ) = 0;
  virtual void visitFactorDivides( FactorDivides& host ) = 0;
};

class Factor::ConstVisitor{
public:
  virtual void visitFactorTerm( FactorTerm const& host ) = 0;
  virtual void visitFactorTimes( FactorTimes const& host ) = 0;
  virtual void visitFactorDivides( FactorDivides const& host ) = 0;
};

class FamilyName::Visitor{
public:
  virtual void visitFamilyNameEps( FamilyNameEps& host ) = 0;
  virtual void visitFamilyNameFamily( FamilyNameFamily& host ) = 0;
};

class FamilyName::ConstVisitor{
public:
  virtual void visitFamilyNameEps( FamilyNameEps const& host ) = 0;
  virtual void visitFamilyNameFamily( FamilyNameFamily const& host ) = 0;
};

class FromClause::Visitor{
public:
  virtual void visitFrom( From& host ) = 0;
  virtual void visitFromWithColumnDefs( FromWithColumnDefs& host ) = 0;
};

class FromClause::ConstVisitor{
public:
  virtual void visitFrom( From const& host ) = 0;
  virtual void visitFromWithColumnDefs( FromWithColumnDefs const& host ) = 0;
};

class GroupByClause::Visitor{
public:
  virtual void visitGroupByClauseEps( GroupByClauseEps& host ) = 0;
  virtual void visitGroupByClauseGroupBy( GroupByClauseGroupBy& host ) = 0;
};

class GroupByClause::ConstVisitor{
public:
  virtual void visitGroupByClauseEps( GroupByClauseEps const& host ) = 0;
  virtual void visitGroupByClauseGroupBy( GroupByClauseGroupBy const& host ) = 0;
};

class HavingClause::Visitor{
public:
  virtual void visitHavingClauseEps( HavingClauseEps& host ) = 0;
  virtual void visitHavingClauseHaving( HavingClauseHaving& host ) = 0;
};

class HavingClause::ConstVisitor{
public:
  virtual void visitHavingClauseEps( HavingClauseEps const& host ) = 0;
  virtual void visitHavingClauseHaving( HavingClauseHaving const& host ) = 0;
};

class InCondition::Visitor{
public:
  virtual void visitInConditionIn( InConditionIn& host ) = 0;
  virtual void visitInConditionNotIn( InConditionNotIn& host ) = 0;
};

class InCondition::ConstVisitor{
public:
  virtual void visitInConditionIn( InConditionIn const& host ) = 0;
  virtual void visitInConditionNotIn( InConditionNotIn const& host ) = 0;
};

class IsNullCondition::Visitor{
public:
  virtual void visitIsNullConditionIsNull( IsNullConditionIsNull& host ) = 0;
  virtual void visitIsNullConditionIsNotNull( IsNullConditionIsNotNull& host ) = 0;
};

class IsNullCondition::ConstVisitor{
public:
  virtual void visitIsNullConditionIsNull( IsNullConditionIsNull const& host ) = 0;
  virtual void visitIsNullConditionIsNotNull( IsNullConditionIsNotNull const& host ) = 0;
};

class LikeCondition::Visitor{
public:
  virtual void visitLikeConditionLike( LikeConditionLike& host ) = 0;
  virtual void visitLikeConditionNotLike( LikeConditionNotLike& host ) = 0;
};

class LikeCondition::ConstVisitor{
public:
  virtual void visitLikeConditionLike( LikeConditionLike const& host ) = 0;
  virtual void visitLikeConditionNotLike( LikeConditionNotLike const& host ) = 0;
};

class LimitClause::Visitor{
public:
  virtual void visitLimitClauseEps( LimitClauseEps& host ) = 0;
  virtual void visitLimitClauseLimitBindParameter( LimitClauseLimitBindParameter& host ) = 0;
  virtual void visitLimitClauseLimitNumber( LimitClauseLimitNumber& host ) = 0;
};

class LimitClause::ConstVisitor{
public:
  virtual void visitLimitClauseEps( LimitClauseEps const& host ) = 0;
  virtual void visitLimitClauseLimitBindParameter( LimitClauseLimitBindParameter const& host ) = 0;
  virtual void visitLimitClauseLimitNumber( LimitClauseLimitNumber const& host ) = 0;
};

class Nullability::Visitor{
public:
  virtual void visitNullabilityEps( NullabilityEps& host ) = 0;
  virtual void visitNullabilityNull( NullabilityNull& host ) = 0;
  virtual void visitNullabilityNotNull( NullabilityNotNull& host ) = 0;
};

class Nullability::ConstVisitor{
public:
  virtual void visitNullabilityEps( NullabilityEps const& host ) = 0;
  virtual void visitNullabilityNull( NullabilityNull const& host ) = 0;
  virtual void visitNullabilityNotNull( NullabilityNotNull const& host ) = 0;
};

class Nulls::Visitor{
public:
  virtual void visitNullsEps( NullsEps& host ) = 0;
  virtual void visitNullsFirst( NullsFirst& host ) = 0;
  virtual void visitNullsLast( NullsLast& host ) = 0;
};

class Nulls::ConstVisitor{
public:
  virtual void visitNullsEps( NullsEps const& host ) = 0;
  virtual void visitNullsFirst( NullsFirst const& host ) = 0;
  virtual void visitNullsLast( NullsLast const& host ) = 0;
};

class Number::Visitor{
public:
  virtual void visitNumberInteger( NumberInteger& host ) = 0;
};

class Number::ConstVisitor{
public:
  virtual void visitNumberInteger( NumberInteger const& host ) = 0;
};

class Operand::Visitor{
public:
  virtual void visitOperandSummand( OperandSummand& host ) = 0;
  virtual void visitOperandAppend( OperandAppend& host ) = 0;
};

class Operand::ConstVisitor{
public:
  virtual void visitOperandSummand( OperandSummand const& host ) = 0;
  virtual void visitOperandAppend( OperandAppend const& host ) = 0;
};

class Operands::Visitor{
public:
  virtual void visitOperandsLast( OperandsLast& host ) = 0;
  virtual void visitOperandsCons( OperandsCons& host ) = 0;
};

class Operands::ConstVisitor{
public:
  virtual void visitOperandsLast( OperandsLast const& host ) = 0;
  virtual void visitOperandsCons( OperandsCons const& host ) = 0;
};

class Order::Visitor{
public:
  virtual void visitOrderOrder( OrderOrder& host ) = 0;
};

class Order::ConstVisitor{
public:
  virtual void visitOrderOrder( OrderOrder const& host ) = 0;
};

class OrderByClause::Visitor{
public:
  virtual void visitOrderByClauseEps( OrderByClauseEps& host ) = 0;
  virtual void visitOrderByClauseOrderBy( OrderByClauseOrderBy& host ) = 0;
};

class OrderByClause::ConstVisitor{
public:
  virtual void visitOrderByClauseEps( OrderByClauseEps const& host ) = 0;
  virtual void visitOrderByClauseOrderBy( OrderByClauseOrderBy const& host ) = 0;
};

class Orders::Visitor{
public:
  virtual void visitOrdersLast( OrdersLast& host ) = 0;
  virtual void visitOrdersCons( OrdersCons& host ) = 0;
};

class Orders::ConstVisitor{
public:
  virtual void visitOrdersLast( OrdersLast const& host ) = 0;
  virtual void visitOrdersCons( OrdersCons const& host ) = 0;
};

class PrimaryKey::Visitor{
public:
  virtual void visitPrimaryKeyEps( PrimaryKeyEps& host ) = 0;
  virtual void visitPrimaryKeyPrimaryKey( PrimaryKeyPrimaryKey& host ) = 0;
};

class PrimaryKey::ConstVisitor{
public:
  virtual void visitPrimaryKeyEps( PrimaryKeyEps const& host ) = 0;
  virtual void visitPrimaryKeyPrimaryKey( PrimaryKeyPrimaryKey const& host ) = 0;
};

class RowValueConstructor::Visitor{
public:
  virtual void visitRowValue( RowValue& host ) = 0;
};

class RowValueConstructor::ConstVisitor{
public:
  virtual void visitRowValue( RowValue const& host ) = 0;
};

class SchemaName::Visitor{
public:
  virtual void visitSchemaNameEps( SchemaNameEps& host ) = 0;
  virtual void visitSchemaNameSchema( SchemaNameSchema& host ) = 0;
};

class SchemaName::ConstVisitor{
public:
  virtual void visitSchemaNameEps( SchemaNameEps const& host ) = 0;
  virtual void visitSchemaNameSchema( SchemaNameSchema const& host ) = 0;
};

class SelectExpression::Visitor{
public:
  virtual void visitSelectExpressionAny( SelectExpressionAny& host ) = 0;
  virtual void visitSelectExpressionTerm( SelectExpressionTerm& host ) = 0;
  virtual void visitSelectExpressionTermAs( SelectExpressionTermAs& host ) = 0;
};

class SelectExpression::ConstVisitor{
public:
  virtual void visitSelectExpressionAny( SelectExpressionAny const& host ) = 0;
  virtual void visitSelectExpressionTerm( SelectExpressionTerm const& host ) = 0;
  virtual void visitSelectExpressionTermAs( SelectExpressionTermAs const& host ) = 0;
};

class SelectExpressions::Visitor{
public:
  virtual void visitSelectExpressionsLast( SelectExpressionsLast& host ) = 0;
  virtual void visitSelectExpressionsCons( SelectExpressionsCons& host ) = 0;
};

class SelectExpressions::ConstVisitor{
public:
  virtual void visitSelectExpressionsLast( SelectExpressionsLast const& host ) = 0;
  virtual void visitSelectExpressionsCons( SelectExpressionsCons const& host ) = 0;
};

class SelectModifier::Visitor{
public:
  virtual void visitSelectModifierEps( SelectModifierEps& host ) = 0;
  virtual void visitSelectModifierDistinct( SelectModifierDistinct& host ) = 0;
  virtual void visitSelectModifierAll( SelectModifierAll& host ) = 0;
};

class SelectModifier::ConstVisitor{
public:
  virtual void visitSelectModifierEps( SelectModifierEps const& host ) = 0;
  virtual void visitSelectModifierDistinct( SelectModifierDistinct const& host ) = 0;
  virtual void visitSelectModifierAll( SelectModifierAll const& host ) = 0;
};

class Summand::Visitor{
public:
  virtual void visitSummandFactor( SummandFactor& host ) = 0;
  virtual void visitSummandPlus( SummandPlus& host ) = 0;
  virtual void visitSummandMinus( SummandMinus& host ) = 0;
};

class Summand::ConstVisitor{
public:
  virtual void visitSummandFactor( SummandFactor const& host ) = 0;
  virtual void visitSummandPlus( SummandPlus const& host ) = 0;
  virtual void visitSummandMinus( SummandMinus const& host ) = 0;
};

class TableAlias::Visitor{
public:
  virtual void visitTableAliasEps( TableAliasEps& host ) = 0;
  virtual void visitTableAliasAlias( TableAliasAlias& host ) = 0;
};

class TableAlias::ConstVisitor{
public:
  virtual void visitTableAliasEps( TableAliasEps const& host ) = 0;
  virtual void visitTableAliasAlias( TableAliasAlias const& host ) = 0;
};

class TableExpression::Visitor{
public:
  virtual void visitTable( Table& host ) = 0;
};

class TableExpression::ConstVisitor{
public:
  virtual void visitTable( Table const& host ) = 0;
};

class Term::Visitor{
public:
  virtual void visitTermValue( TermValue& host ) = 0;
  virtual void visitTermBindParameter( TermBindParameter& host ) = 0;
  virtual void visitTermCase( TermCase& host ) = 0;
  virtual void visitTermCaseWhen( TermCaseWhen& host ) = 0;
  virtual void visitTermParen( TermParen& host ) = 0;
  virtual void visitTermColumnRef( TermColumnRef& host ) = 0;
  virtual void visitTermRowValueConstructor( TermRowValueConstructor& host ) = 0;
};

class Term::ConstVisitor{
public:
  virtual void visitTermValue( TermValue const& host ) = 0;
  virtual void visitTermBindParameter( TermBindParameter const& host ) = 0;
  virtual void visitTermCase( TermCase const& host ) = 0;
  virtual void visitTermCaseWhen( TermCaseWhen const& host ) = 0;
  virtual void visitTermParen( TermParen const& host ) = 0;
  virtual void visitTermColumnRef( TermColumnRef const& host ) = 0;
  virtual void visitTermRowValueConstructor( TermRowValueConstructor const& host ) = 0;
};

class Terms::Visitor{
public:
  virtual void visitTermsLast( TermsLast& host ) = 0;
  virtual void visitTermsCons( TermsCons& host ) = 0;
};

class Terms::ConstVisitor{
public:
  virtual void visitTermsLast( TermsLast const& host ) = 0;
  virtual void visitTermsCons( TermsCons const& host ) = 0;
};

class Value::Visitor{
public:
  virtual void visitValueString( ValueString& host ) = 0;
  virtual void visitValueBoolean( ValueBoolean& host ) = 0;
  virtual void visitValueTrue( ValueTrue& host ) = 0;
  virtual void visitValueFalse( ValueFalse& host ) = 0;
  virtual void visitValueNumber( ValueNumber& host ) = 0;
  virtual void visitValueNull( ValueNull& host ) = 0;
};

class Value::ConstVisitor{
public:
  virtual void visitValueString( ValueString const& host ) = 0;
  virtual void visitValueBoolean( ValueBoolean const& host ) = 0;
  virtual void visitValueTrue( ValueTrue const& host ) = 0;
  virtual void visitValueFalse( ValueFalse const& host ) = 0;
  virtual void visitValueNumber( ValueNumber const& host ) = 0;
  virtual void visitValueNull( ValueNull const& host ) = 0;
};

class WhenClause::Visitor{
public:
  virtual void visitWhen( When& host ) = 0;
};

class WhenClause::ConstVisitor{
public:
  virtual void visitWhen( When const& host ) = 0;
};

class WhenClauses::Visitor{
public:
  virtual void visitWhenClausesLast( WhenClausesLast& host ) = 0;
  virtual void visitWhenClausesCons( WhenClausesCons& host ) = 0;
};

class WhenClauses::ConstVisitor{
public:
  virtual void visitWhenClausesLast( WhenClausesLast const& host ) = 0;
  virtual void visitWhenClausesCons( WhenClausesCons const& host ) = 0;
};

class WhereClause::Visitor{
public:
  virtual void visitWhereClauseEps( WhereClauseEps& host ) = 0;
  virtual void visitWhereClauseWhere( WhereClauseWhere& host ) = 0;
};

class WhereClause::ConstVisitor{
public:
  virtual void visitWhereClauseEps( WhereClauseEps const& host ) = 0;
  virtual void visitWhereClauseWhere( WhereClauseWhere const& host ) = 0;
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream& out, Query const& self );
std::ostream& operator <<( std::ostream& out, AndCondition const& self );
std::ostream& operator <<( std::ostream& out, As const& self );
std::ostream& operator <<( std::ostream& out, AscDesc const& self );
std::ostream& operator <<( std::ostream& out, BetweenCondition const& self );
std::ostream& operator <<( std::ostream& out, BindParameter const& self );
std::ostream& operator <<( std::ostream& out, CaseClause const& self );
std::ostream& operator <<( std::ostream& out, CaseWhenClause const& self );
std::ostream& operator <<( std::ostream& out, ColumnDef const& self );
std::ostream& operator <<( std::ostream& out, ColumnDefs const& self );
std::ostream& operator <<( std::ostream& out, ColumnRef const& self );
std::ostream& operator <<( std::ostream& out, Compare const& self );
std::ostream& operator <<( std::ostream& out, Condition const& self );
std::ostream& operator <<( std::ostream& out, DataType const& self );
std::ostream& operator <<( std::ostream& out, ElseClause const& self );
std::ostream& operator <<( std::ostream& out, Expression const& self );
std::ostream& operator <<( std::ostream& out, Expressions const& self );
std::ostream& operator <<( std::ostream& out, Factor const& self );
std::ostream& operator <<( std::ostream& out, FamilyName const& self );
std::ostream& operator <<( std::ostream& out, FromClause const& self );
std::ostream& operator <<( std::ostream& out, GroupByClause const& self );
std::ostream& operator <<( std::ostream& out, HavingClause const& self );
std::ostream& operator <<( std::ostream& out, InCondition const& self );
std::ostream& operator <<( std::ostream& out, IsNullCondition const& self );
std::ostream& operator <<( std::ostream& out, LikeCondition const& self );
std::ostream& operator <<( std::ostream& out, LimitClause const& self );
std::ostream& operator <<( std::ostream& out, Nullability const& self );
std::ostream& operator <<( std::ostream& out, Nulls const& self );
std::ostream& operator <<( std::ostream& out, Number const& self );
std::ostream& operator <<( std::ostream& out, Operand const& self );
std::ostream& operator <<( std::ostream& out, Operands const& self );
std::ostream& operator <<( std::ostream& out, Order const& self );
std::ostream& operator <<( std::ostream& out, OrderByClause const& self );
std::ostream& operator <<( std::ostream& out, Orders const& self );
std::ostream& operator <<( std::ostream& out, PrimaryKey const& self );
std::ostream& operator <<( std::ostream& out, RowValueConstructor const& self );
std::ostream& operator <<( std::ostream& out, SchemaName const& self );
std::ostream& operator <<( std::ostream& out, SelectExpression const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressions const& self );
std::ostream& operator <<( std::ostream& out, SelectModifier const& self );
std::ostream& operator <<( std::ostream& out, Summand const& self );
std::ostream& operator <<( std::ostream& out, TableAlias const& self );
std::ostream& operator <<( std::ostream& out, TableExpression const& self );
std::ostream& operator <<( std::ostream& out, Term const& self );
std::ostream& operator <<( std::ostream& out, Terms const& self );
std::ostream& operator <<( std::ostream& out, Value const& self );
std::ostream& operator <<( std::ostream& out, WhenClause const& self );
std::ostream& operator <<( std::ostream& out, WhenClauses const& self );
std::ostream& operator <<( std::ostream& out, WhereClause const& self );
std::ostream& operator <<( std::ostream& out, QuerySelect const& self );
std::ostream& operator <<( std::ostream& out, AndConditionCondition const& self );
std::ostream& operator <<( std::ostream& out, AndConditionAnd const& self );
std::ostream& operator <<( std::ostream& out, AsEps const& self );
std::ostream& operator <<( std::ostream& out, AsAs const& self );
std::ostream& operator <<( std::ostream& out, AscDescEps const& self );
std::ostream& operator <<( std::ostream& out, AscDescAsc const& self );
std::ostream& operator <<( std::ostream& out, AscDescDesc const& self );
std::ostream& operator <<( std::ostream& out, BetweenConditionBetween const& self );
std::ostream& operator <<( std::ostream& out, BetweenConditionNotBetween const& self );
std::ostream& operator <<( std::ostream& out, BindParameterQuestion const& self );
std::ostream& operator <<( std::ostream& out, BindParameterRef const& self );
std::ostream& operator <<( std::ostream& out, Case const& self );
std::ostream& operator <<( std::ostream& out, CaseWhen const& self );
std::ostream& operator <<( std::ostream& out, ColumnDefinition const& self );
std::ostream& operator <<( std::ostream& out, ColumnDefsLast const& self );
std::ostream& operator <<( std::ostream& out, ColumnDefsCons const& self );
std::ostream& operator <<( std::ostream& out, Column const& self );
std::ostream& operator <<( std::ostream& out, CompareNE const& self );
std::ostream& operator <<( std::ostream& out, CompareLE const& self );
std::ostream& operator <<( std::ostream& out, CompareGE const& self );
std::ostream& operator <<( std::ostream& out, CompareEQ const& self );
std::ostream& operator <<( std::ostream& out, CompareLT const& self );
std::ostream& operator <<( std::ostream& out, CompareGT const& self );
std::ostream& operator <<( std::ostream& out, ConditionCompare const& self );
std::ostream& operator <<( std::ostream& out, ConditionInCondition const& self );
std::ostream& operator <<( std::ostream& out, ConditionLikeCondition const& self );
std::ostream& operator <<( std::ostream& out, ConditionBetweenCondition const& self );
std::ostream& operator <<( std::ostream& out, ConditionIsNullCondition const& self );
std::ostream& operator <<( std::ostream& out, ConditionNotExpression const& self );
std::ostream& operator <<( std::ostream& out, ConditionParen const& self );
std::ostream& operator <<( std::ostream& out, CharType const& self );
std::ostream& operator <<( std::ostream& out, IntegerType const& self );
std::ostream& operator <<( std::ostream& out, BooleanType const& self );
std::ostream& operator <<( std::ostream& out, FloatType const& self );
std::ostream& operator <<( std::ostream& out, DateType const& self );
std::ostream& operator <<( std::ostream& out, ElseClauseEps const& self );
std::ostream& operator <<( std::ostream& out, ElseClauseElse const& self );
std::ostream& operator <<( std::ostream& out, ExpressionAndCondition const& self );
std::ostream& operator <<( std::ostream& out, ExpressionOr const& self );
std::ostream& operator <<( std::ostream& out, ExpressionsLast const& self );
std::ostream& operator <<( std::ostream& out, ExpressionsCons const& self );
std::ostream& operator <<( std::ostream& out, FactorTerm const& self );
std::ostream& operator <<( std::ostream& out, FactorTimes const& self );
std::ostream& operator <<( std::ostream& out, FactorDivides const& self );
std::ostream& operator <<( std::ostream& out, FamilyNameEps const& self );
std::ostream& operator <<( std::ostream& out, FamilyNameFamily const& self );
std::ostream& operator <<( std::ostream& out, From const& self );
std::ostream& operator <<( std::ostream& out, FromWithColumnDefs const& self );
std::ostream& operator <<( std::ostream& out, GroupByClauseEps const& self );
std::ostream& operator <<( std::ostream& out, GroupByClauseGroupBy const& self );
std::ostream& operator <<( std::ostream& out, HavingClauseEps const& self );
std::ostream& operator <<( std::ostream& out, HavingClauseHaving const& self );
std::ostream& operator <<( std::ostream& out, InConditionIn const& self );
std::ostream& operator <<( std::ostream& out, InConditionNotIn const& self );
std::ostream& operator <<( std::ostream& out, IsNullConditionIsNull const& self );
std::ostream& operator <<( std::ostream& out, IsNullConditionIsNotNull const& self );
std::ostream& operator <<( std::ostream& out, LikeConditionLike const& self );
std::ostream& operator <<( std::ostream& out, LikeConditionNotLike const& self );
std::ostream& operator <<( std::ostream& out, LimitClauseEps const& self );
std::ostream& operator <<( std::ostream& out, LimitClauseLimitBindParameter const& self );
std::ostream& operator <<( std::ostream& out, LimitClauseLimitNumber const& self );
std::ostream& operator <<( std::ostream& out, NullabilityEps const& self );
std::ostream& operator <<( std::ostream& out, NullabilityNull const& self );
std::ostream& operator <<( std::ostream& out, NullabilityNotNull const& self );
std::ostream& operator <<( std::ostream& out, NullsEps const& self );
std::ostream& operator <<( std::ostream& out, NullsFirst const& self );
std::ostream& operator <<( std::ostream& out, NullsLast const& self );
std::ostream& operator <<( std::ostream& out, NumberInteger const& self );
std::ostream& operator <<( std::ostream& out, OperandSummand const& self );
std::ostream& operator <<( std::ostream& out, OperandAppend const& self );
std::ostream& operator <<( std::ostream& out, OperandsLast const& self );
std::ostream& operator <<( std::ostream& out, OperandsCons const& self );
std::ostream& operator <<( std::ostream& out, OrderOrder const& self );
std::ostream& operator <<( std::ostream& out, OrderByClauseEps const& self );
std::ostream& operator <<( std::ostream& out, OrderByClauseOrderBy const& self );
std::ostream& operator <<( std::ostream& out, OrdersLast const& self );
std::ostream& operator <<( std::ostream& out, OrdersCons const& self );
std::ostream& operator <<( std::ostream& out, PrimaryKeyEps const& self );
std::ostream& operator <<( std::ostream& out, PrimaryKeyPrimaryKey const& self );
std::ostream& operator <<( std::ostream& out, RowValue const& self );
std::ostream& operator <<( std::ostream& out, SchemaNameEps const& self );
std::ostream& operator <<( std::ostream& out, SchemaNameSchema const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressionAny const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressionTerm const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressionTermAs const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressionsLast const& self );
std::ostream& operator <<( std::ostream& out, SelectExpressionsCons const& self );
std::ostream& operator <<( std::ostream& out, SelectModifierEps const& self );
std::ostream& operator <<( std::ostream& out, SelectModifierDistinct const& self );
std::ostream& operator <<( std::ostream& out, SelectModifierAll const& self );
std::ostream& operator <<( std::ostream& out, SummandFactor const& self );
std::ostream& operator <<( std::ostream& out, SummandPlus const& self );
std::ostream& operator <<( std::ostream& out, SummandMinus const& self );
std::ostream& operator <<( std::ostream& out, TableAliasEps const& self );
std::ostream& operator <<( std::ostream& out, TableAliasAlias const& self );
std::ostream& operator <<( std::ostream& out, Table const& self );
std::ostream& operator <<( std::ostream& out, TermValue const& self );
std::ostream& operator <<( std::ostream& out, TermBindParameter const& self );
std::ostream& operator <<( std::ostream& out, TermCase const& self );
std::ostream& operator <<( std::ostream& out, TermCaseWhen const& self );
std::ostream& operator <<( std::ostream& out, TermParen const& self );
std::ostream& operator <<( std::ostream& out, TermColumnRef const& self );
std::ostream& operator <<( std::ostream& out, TermRowValueConstructor const& self );
std::ostream& operator <<( std::ostream& out, TermsLast const& self );
std::ostream& operator <<( std::ostream& out, TermsCons const& self );
std::ostream& operator <<( std::ostream& out, ValueString const& self );
std::ostream& operator <<( std::ostream& out, ValueBoolean const& self );
std::ostream& operator <<( std::ostream& out, ValueTrue const& self );
std::ostream& operator <<( std::ostream& out, ValueFalse const& self );
std::ostream& operator <<( std::ostream& out, ValueNumber const& self );
std::ostream& operator <<( std::ostream& out, ValueNull const& self );
std::ostream& operator <<( std::ostream& out, When const& self );
std::ostream& operator <<( std::ostream& out, WhenClausesLast const& self );
std::ostream& operator <<( std::ostream& out, WhenClausesCons const& self );
std::ostream& operator <<( std::ostream& out, WhereClauseEps const& self );
std::ostream& operator <<( std::ostream& out, WhereClauseWhere const& self );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// automaton nodes

class Node1 {
public:
  explicit Node1();
};

class Node2 {
public:
  std::shared_ptr< Query > arg1;
  explicit Node2( std::shared_ptr< Query > const& arg1_ );
};

class Node3 {
public:
  std::shared_ptr< AndCondition > arg1;
  explicit Node3( std::shared_ptr< AndCondition > const& arg1_ );
};

class Node4 {
public:
  explicit Node4();
};

class Node5 {
public:
  explicit Node5();
};

class Node6 {
public:
  explicit Node6();
};

class Node7 {
public:
  explicit Node7();
};

class Node8 {
public:
  explicit Node8();
};

class Node9 {
public:
  explicit Node9();
};

class Node10 {
public:
  explicit Node10();
};

class Node11 {
public:
  explicit Node11();
};

class Node12 {
public:
  explicit Node12();
};

class Node13 {
public:
  explicit Node13();
};

class Node14 {
public:
  explicit Node14();
};

class Node15 {
public:
  explicit Node15();
};

class Node16 {
public:
  std::shared_ptr< Condition > arg1;
  explicit Node16( std::shared_ptr< Condition > const& arg1_ );
};

class Node17 {
public:
  explicit Node17();
};

class Node18 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node18( std::shared_ptr< Term > const& arg1_ );
};

class Node19 {
public:
  std::string arg1;
  explicit Node19( std::string const& arg1_ );
};

class Node20 {
public:
  explicit Node20();
};

class Node21 {
public:
  explicit Node21();
};

class Node22 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node22( std::shared_ptr< Expression > const& arg1_ );
};

class Node23 {
public:
  explicit Node23();
};

class Node24 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node24( std::shared_ptr< Operand > const& arg1_ );
};

class Node25 {
public:
  explicit Node25();
};

class Node26 {
public:
  explicit Node26();
};

class Node27 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node27( std::shared_ptr< Operand > const& arg1_ );
};

class Node28 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node28( std::shared_ptr< Operand > const& arg1_ );
};

class Node29 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node29( std::shared_ptr< Operand > const& arg1_ );
};

class Node30 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node30( std::shared_ptr< Operand > const& arg1_ );
};

class Node31 {
public:
  explicit Node31();
};

class Node32 {
public:
  explicit Node32();
};

class Node33 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node33( std::shared_ptr< Operand > const& arg1_ );
};

class Node34 {
public:
  explicit Node34();
};

class Node35 {
public:
  explicit Node35();
};

class Node36 {
public:
  std::shared_ptr< SelectModifier > arg1;
  explicit Node36( std::shared_ptr< SelectModifier > const& arg1_ );
};

class Node37 {
public:
  explicit Node37();
};

class Node38 {
public:
  explicit Node38();
};

class Node39 {
public:
  explicit Node39();
};

class Node40 {
public:
  explicit Node40();
};

class Node41 {
public:
  explicit Node41();
};

class Node42 {
public:
  explicit Node42();
};

class Node43 {
public:
  std::shared_ptr< Compare > arg1;
  explicit Node43( std::shared_ptr< Compare > const& arg1_ );
};

class Node44 {
public:
  explicit Node44();
};

class Node45 {
public:
  explicit Node45();
};

class Node46 {
public:
  explicit Node46();
};

class Node47 {
public:
  explicit Node47();
};

class Node48 {
public:
  explicit Node48();
};

class Node49 {
public:
  explicit Node49();
};

class Node50 {
public:
  explicit Node50();
};

class Node51 {
public:
  explicit Node51();
};

class Node52 {
public:
  explicit Node52();
};

class Node53 {
public:
  explicit Node53();
};

class Node54 {
public:
  explicit Node54();
};

class Node55 {
public:
  explicit Node55();
};

class Node56 {
public:
  int arg1;
  explicit Node56( int const& arg1_ );
};

class Node57 {
public:
  explicit Node57();
};

class Node58 {
public:
  std::shared_ptr< ColumnRef > arg1;
  explicit Node58( std::shared_ptr< ColumnRef > const& arg1_ );
};

class Node59 {
public:
  explicit Node59();
};

class Node60 {
public:
  std::shared_ptr< WhenClauses > arg1;
  explicit Node60( std::shared_ptr< WhenClauses > const& arg1_ );
};

class Node61 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node61( std::shared_ptr< Term > const& arg1_ );
};

class Node62 {
public:
  std::shared_ptr< ElseClause > arg1;
  explicit Node62( std::shared_ptr< ElseClause > const& arg1_ );
};

class Node63 {
public:
  explicit Node63();
};

class Node64 {
public:
  std::shared_ptr< WhenClauses > arg1;
  explicit Node64( std::shared_ptr< WhenClauses > const& arg1_ );
};

class Node65 {
public:
  std::shared_ptr< ElseClause > arg1;
  explicit Node65( std::shared_ptr< ElseClause > const& arg1_ );
};

class Node66 {
public:
  int arg1;
  explicit Node66( int const& arg1_ );
};

class Node67 {
public:
  std::string arg1;
  explicit Node67( std::string const& arg1_ );
};

class Node68 {
public:
  explicit Node68();
};

class Node69 {
public:
  std::shared_ptr< TableExpression > arg1;
  explicit Node69( std::shared_ptr< TableExpression > const& arg1_ );
};

class Node70 {
public:
  std::shared_ptr< FamilyName > arg1;
  explicit Node70( std::shared_ptr< FamilyName > const& arg1_ );
};

class Node71 {
public:
  std::shared_ptr< FamilyName > arg1;
  explicit Node71( std::shared_ptr< FamilyName > const& arg1_ );
};

class Node72 {
public:
  std::shared_ptr< PrimaryKey > arg1;
  explicit Node72( std::shared_ptr< PrimaryKey > const& arg1_ );
};

class Node73 {
public:
  std::shared_ptr< DataType > arg1;
  explicit Node73( std::shared_ptr< DataType > const& arg1_ );
};

class Node74 {
public:
  std::shared_ptr< Nullability > arg1;
  explicit Node74( std::shared_ptr< Nullability > const& arg1_ );
};

class Node75 {
public:
  std::shared_ptr< ColumnDefs > arg1;
  explicit Node75( std::shared_ptr< ColumnDefs > const& arg1_ );
};

class Node76 {
public:
  std::shared_ptr< ColumnDef > arg1;
  explicit Node76( std::shared_ptr< ColumnDef > const& arg1_ );
};

class Node77 {
public:
  explicit Node77();
};

class Node78 {
public:
  explicit Node78();
};

class Node79 {
public:
  explicit Node79();
};

class Node80 {
public:
  explicit Node80();
};

class Node81 {
public:
  explicit Node81();
};

class Node82 {
public:
  explicit Node82();
};

class Node83 {
public:
  std::shared_ptr< BetweenCondition > arg1;
  explicit Node83( std::shared_ptr< BetweenCondition > const& arg1_ );
};

class Node84 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node84( std::shared_ptr< Operand > const& arg1_ );
};

class Node85 {
public:
  std::shared_ptr< InCondition > arg1;
  explicit Node85( std::shared_ptr< InCondition > const& arg1_ );
};

class Node86 {
public:
  std::shared_ptr< IsNullCondition > arg1;
  explicit Node86( std::shared_ptr< IsNullCondition > const& arg1_ );
};

class Node87 {
public:
  std::shared_ptr< LikeCondition > arg1;
  explicit Node87( std::shared_ptr< LikeCondition > const& arg1_ );
};

class Node88 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node88( std::shared_ptr< Expression > const& arg1_ );
};

class Node89 {
public:
  explicit Node89();
};

class Node90 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node90( std::shared_ptr< Expression > const& arg1_ );
};

class Node91 {
public:
  explicit Node91();
};

class Node92 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node92( std::shared_ptr< Expression > const& arg1_ );
};

class Node93 {
public:
  std::shared_ptr< AndCondition > arg1;
  explicit Node93( std::shared_ptr< AndCondition > const& arg1_ );
};

class Node94 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node94( std::shared_ptr< Expression > const& arg1_ );
};

class Node95 {
public:
  std::shared_ptr< Expressions > arg1;
  explicit Node95( std::shared_ptr< Expressions > const& arg1_ );
};

class Node96 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node96( std::shared_ptr< Expression > const& arg1_ );
};

class Node97 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node97( std::shared_ptr< Term > const& arg1_ );
};

class Node98 {
public:
  std::shared_ptr< Factor > arg1;
  explicit Node98( std::shared_ptr< Factor > const& arg1_ );
};

class Node99 {
public:
  std::shared_ptr< Factor > arg1;
  explicit Node99( std::shared_ptr< Factor > const& arg1_ );
};

class Node100 {
public:
  std::shared_ptr< Factor > arg1;
  explicit Node100( std::shared_ptr< Factor > const& arg1_ );
};

class Node101 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node101( std::shared_ptr< Term > const& arg1_ );
};

class Node102 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node102( std::shared_ptr< Term > const& arg1_ );
};

class Node103 {
public:
  explicit Node103();
};

class Node104 {
public:
  std::string arg1;
  explicit Node104( std::string const& arg1_ );
};

class Node105 {
public:
  explicit Node105();
};

class Node106 {
public:
  explicit Node106();
};

class Node107 {
public:
  std::shared_ptr< SelectExpressions > arg1;
  explicit Node107( std::shared_ptr< SelectExpressions > const& arg1_ );
};

class Node108 {
public:
  std::shared_ptr< ColumnDefs > arg1;
  explicit Node108( std::shared_ptr< ColumnDefs > const& arg1_ );
};

class Node109 {
public:
  std::shared_ptr< WhereClause > arg1;
  explicit Node109( std::shared_ptr< WhereClause > const& arg1_ );
};

class Node110 {
public:
  std::shared_ptr< Expressions > arg1;
  explicit Node110( std::shared_ptr< Expressions > const& arg1_ );
};

class Node111 {
public:
  explicit Node111();
};

class Node112 {
public:
  std::shared_ptr< GroupByClause > arg1;
  explicit Node112( std::shared_ptr< GroupByClause > const& arg1_ );
};

class Node113 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node113( std::shared_ptr< Expression > const& arg1_ );
};

class Node114 {
public:
  std::shared_ptr< Operands > arg1;
  explicit Node114( std::shared_ptr< Operands > const& arg1_ );
};

class Node115 {
public:
  std::shared_ptr< Operands > arg1;
  explicit Node115( std::shared_ptr< Operands > const& arg1_ );
};

class Node116 {
public:
  explicit Node116();
};

class Node117 {
public:
  explicit Node117();
};

class Node118 {
public:
  explicit Node118();
};

class Node119 {
public:
  explicit Node119();
};

class Node120 {
public:
  explicit Node120();
};

class Node121 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node121( std::shared_ptr< Operand > const& arg1_ );
};

class Node122 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node122( std::shared_ptr< Operand > const& arg1_ );
};

class Node123 {
public:
  std::shared_ptr< OrderByClause > arg1;
  explicit Node123( std::shared_ptr< OrderByClause > const& arg1_ );
};

class Node124 {
public:
  std::shared_ptr< BindParameter > arg1;
  explicit Node124( std::shared_ptr< BindParameter > const& arg1_ );
};

class Node125 {
public:
  std::shared_ptr< Number > arg1;
  explicit Node125( std::shared_ptr< Number > const& arg1_ );
};

class Node126 {
public:
  explicit Node126();
};

class Node127 {
public:
  explicit Node127();
};

class Node128 {
public:
  explicit Node128();
};

class Node129 {
public:
  std::shared_ptr< AscDesc > arg1;
  explicit Node129( std::shared_ptr< AscDesc > const& arg1_ );
};

class Node130 {
public:
  explicit Node130();
};

class Node131 {
public:
  explicit Node131();
};

class Node132 {
public:
  explicit Node132();
};

class Node133 {
public:
  int arg1;
  explicit Node133( int const& arg1_ );
};

class Node134 {
public:
  std::shared_ptr< Summand > arg1;
  explicit Node134( std::shared_ptr< Summand > const& arg1_ );
};

class Node135 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node135( std::shared_ptr< Operand > const& arg1_ );
};

class Node136 {
public:
  std::shared_ptr< Operand > arg1;
  explicit Node136( std::shared_ptr< Operand > const& arg1_ );
};

class Node137 {
public:
  std::shared_ptr< Summand > arg1;
  explicit Node137( std::shared_ptr< Summand > const& arg1_ );
};

class Node138 {
public:
  std::shared_ptr< Operands > arg1;
  explicit Node138( std::shared_ptr< Operands > const& arg1_ );
};

class Node139 {
public:
  std::shared_ptr< HavingClause > arg1;
  explicit Node139( std::shared_ptr< HavingClause > const& arg1_ );
};

class Node140 {
public:
  std::shared_ptr< Orders > arg1;
  explicit Node140( std::shared_ptr< Orders > const& arg1_ );
};

class Node141 {
public:
  explicit Node141();
};

class Node142 {
public:
  std::shared_ptr< Nulls > arg1;
  explicit Node142( std::shared_ptr< Nulls > const& arg1_ );
};

class Node143 {
public:
  std::shared_ptr< Orders > arg1;
  explicit Node143( std::shared_ptr< Orders > const& arg1_ );
};

class Node144 {
public:
  std::shared_ptr< Order > arg1;
  explicit Node144( std::shared_ptr< Order > const& arg1_ );
};

class Node145 {
public:
  std::shared_ptr< AscDesc > arg1;
  explicit Node145( std::shared_ptr< AscDesc > const& arg1_ );
};

class Node146 {
public:
  explicit Node146();
};

class Node147 {
public:
  std::shared_ptr< LimitClause > arg1;
  explicit Node147( std::shared_ptr< LimitClause > const& arg1_ );
};

class Node148 {
public:
  explicit Node148();
};

class Node149 {
public:
  std::shared_ptr< FromClause > arg1;
  explicit Node149( std::shared_ptr< FromClause > const& arg1_ );
};

class Node150 {
public:
  explicit Node150();
};

class Node151 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node151( std::shared_ptr< Term > const& arg1_ );
};

class Node152 {
public:
  std::shared_ptr< Terms > arg1;
  explicit Node152( std::shared_ptr< Terms > const& arg1_ );
};

class Node153 {
public:
  explicit Node153();
};

class Node154 {
public:
  std::string arg1;
  explicit Node154( std::string const& arg1_ );
};

class Node155 {
public:
  explicit Node155();
};

class Node156 {
public:
  std::string arg1;
  explicit Node156( std::string const& arg1_ );
};

class Node157 {
public:
  std::shared_ptr< As > arg1;
  explicit Node157( std::shared_ptr< As > const& arg1_ );
};

class Node158 {
public:
  std::shared_ptr< SelectExpressions > arg1;
  explicit Node158( std::shared_ptr< SelectExpressions > const& arg1_ );
};

class Node159 {
public:
  std::shared_ptr< SelectExpression > arg1;
  explicit Node159( std::shared_ptr< SelectExpression > const& arg1_ );
};

class Node160 {
public:
  explicit Node160();
};

class Node161 {
public:
  explicit Node161();
};

class Node162 {
public:
  std::shared_ptr< TableAlias > arg1;
  explicit Node162( std::shared_ptr< TableAlias > const& arg1_ );
};

class Node163 {
public:
  std::shared_ptr< SchemaName > arg1;
  explicit Node163( std::shared_ptr< SchemaName > const& arg1_ );
};

class Node164 {
public:
  std::string arg1;
  explicit Node164( std::string const& arg1_ );
};

class Node165 {
public:
  std::shared_ptr< As > arg1;
  explicit Node165( std::shared_ptr< As > const& arg1_ );
};

class Node166 {
public:
  std::shared_ptr< BindParameter > arg1;
  explicit Node166( std::shared_ptr< BindParameter > const& arg1_ );
};

class Node167 {
public:
  std::shared_ptr< CaseClause > arg1;
  explicit Node167( std::shared_ptr< CaseClause > const& arg1_ );
};

class Node168 {
public:
  std::shared_ptr< CaseWhenClause > arg1;
  explicit Node168( std::shared_ptr< CaseWhenClause > const& arg1_ );
};

class Node169 {
public:
  std::shared_ptr< ColumnRef > arg1;
  explicit Node169( std::shared_ptr< ColumnRef > const& arg1_ );
};

class Node170 {
public:
  explicit Node170();
};

class Node171 {
public:
  std::shared_ptr< RowValueConstructor > arg1;
  explicit Node171( std::shared_ptr< RowValueConstructor > const& arg1_ );
};

class Node172 {
public:
  std::shared_ptr< Value > arg1;
  explicit Node172( std::shared_ptr< Value > const& arg1_ );
};

class Node173 {
public:
  std::shared_ptr< Terms > arg1;
  explicit Node173( std::shared_ptr< Terms > const& arg1_ );
};

class Node174 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node174( std::shared_ptr< Term > const& arg1_ );
};

class Node175 {
public:
  bool arg1;
  explicit Node175( bool const& arg1_ );
};

class Node176 {
public:
  explicit Node176();
};

class Node177 {
public:
  explicit Node177();
};

class Node178 {
public:
  std::shared_ptr< Number > arg1;
  explicit Node178( std::shared_ptr< Number > const& arg1_ );
};

class Node179 {
public:
  std::string arg1;
  explicit Node179( std::string const& arg1_ );
};

class Node180 {
public:
  explicit Node180();
};

class Node181 {
public:
  std::shared_ptr< Term > arg1;
  explicit Node181( std::shared_ptr< Term > const& arg1_ );
};

class Node182 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node182( std::shared_ptr< Expression > const& arg1_ );
};

class Node183 {
public:
  std::shared_ptr< WhenClause > arg1;
  explicit Node183( std::shared_ptr< WhenClause > const& arg1_ );
};

class Node184 {
public:
  std::shared_ptr< WhenClauses > arg1;
  explicit Node184( std::shared_ptr< WhenClauses > const& arg1_ );
};

class Node185 {
public:
  std::shared_ptr< Expression > arg1;
  explicit Node185( std::shared_ptr< Expression > const& arg1_ );
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// parsing state

template< typename... Stack >
class State;

template<>
class State<> {};

template< typename Head, typename... Tail >
class State< Head, Tail... > {
public:
  std::weak_ptr< State< Head, Tail... > > this_;
  Head head;
  std::shared_ptr< State< Tail... > > tail;

private:
  State( Head const& head_, std::shared_ptr< State< Tail... > > const& tail_ );

public:
  static std::shared_ptr< State< Head, Tail... > > make( Head const& head, std::shared_ptr< State< Tail... > > const& tail );

public:
  auto end();
  auto and_();
  auto as_();
  auto asc();
  auto desc();
  auto between();
  auto not_();
  auto question();
  auto ref( int const& arg1 );
  auto case_();
  auto endCase();
  auto comma();
  auto column( std::string const& arg1 );
  auto ne();
  auto le();
  auto ge();
  auto eq();
  auto lt();
  auto gt();
  auto lp();
  auto rp();
  auto charType( int const& arg1 );
  auto integerType();
  auto booleanType();
  auto floatType();
  auto dateType();
  auto else_();
  auto or_();
  auto times();
  auto divides();
  auto family( std::string const& arg1 );
  auto dot();
  auto from_();
  auto group();
  auto by();
  auto having();
  auto in_();
  auto is();
  auto null_();
  auto like();
  auto limit();
  auto nulls();
  auto first_();
  auto last_();
  auto integer( int const& arg1 );
  auto append();
  auto order();
  auto primary();
  auto key();
  auto select();
  auto rawValue();
  auto endRawValue();
  auto schema( std::string const& arg1 );
  auto asterisk();
  auto alias( std::string const& arg1 );
  auto distinct();
  auto all_();
  auto plus();
  auto minus();
  auto table( std::string const& arg1 );
  auto stringValue( std::string const& arg1 );
  auto booleanValue( bool const& arg1 );
  auto true_();
  auto false_();
  auto when_();
  auto then_();
  auto where_();
};

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// transition rules

template< typename... Stack >
auto and__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto as__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto asc_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto desc_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto between_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto not__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto question_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto ref_transition( std::shared_ptr< State< Stack... > > const& src, int const& arg1 );

template< typename... Stack >
auto case__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto endCase_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto comma_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto column_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto ne_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto le_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto ge_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto eq_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto lt_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto gt_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto lp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto rp_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto charType_transition( std::shared_ptr< State< Stack... > > const& src, int const& arg1 );

template< typename... Stack >
auto integerType_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto booleanType_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto floatType_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto dateType_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto else__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto or__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto times_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto divides_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto family_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto dot_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto from__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto group_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto by_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto having_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto in__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto is_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto null__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto like_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto limit_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto nulls_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto first__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto last__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto integer_transition( std::shared_ptr< State< Stack... > > const& src, int const& arg1 );

template< typename... Stack >
auto append_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto order_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto primary_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto key_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto select_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto rawValue_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto endRawValue_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto schema_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto asterisk_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto alias_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto distinct_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto all__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto plus_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto minus_transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto table_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto stringValue_transition( std::shared_ptr< State< Stack... > > const& src, std::string const& arg1 );

template< typename... Stack >
auto booleanValue_transition( std::shared_ptr< State< Stack... > > const& src, bool const& arg1 );

template< typename... Stack >
auto true__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto false__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto when__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto then__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto where__transition( std::shared_ptr< State< Stack... > > const& src );

template< typename... Stack >
auto end_transition( std::shared_ptr< State< Stack... > > const& src );

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin();

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "SQL.hpp.impl"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

#endif

