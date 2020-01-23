
#include "SQL.hpp"

namespace SQL {

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Query::~Query() noexcept {}
AndCondition::~AndCondition() noexcept {}
As::~As() noexcept {}
AscDesc::~AscDesc() noexcept {}
BetweenCondition::~BetweenCondition() noexcept {}
BindParameter::~BindParameter() noexcept {}
CaseClause::~CaseClause() noexcept {}
CaseWhenClause::~CaseWhenClause() noexcept {}
ColumnDef::~ColumnDef() noexcept {}
ColumnDefs::~ColumnDefs() noexcept {}
ColumnRef::~ColumnRef() noexcept {}
Compare::~Compare() noexcept {}
Condition::~Condition() noexcept {}
DataType::~DataType() noexcept {}
ElseClause::~ElseClause() noexcept {}
Expression::~Expression() noexcept {}
Expressions::~Expressions() noexcept {}
Factor::~Factor() noexcept {}
FamilyName::~FamilyName() noexcept {}
FromClause::~FromClause() noexcept {}
GroupByClause::~GroupByClause() noexcept {}
HavingClause::~HavingClause() noexcept {}
InCondition::~InCondition() noexcept {}
IsNullCondition::~IsNullCondition() noexcept {}
LikeCondition::~LikeCondition() noexcept {}
LimitClause::~LimitClause() noexcept {}
Nullability::~Nullability() noexcept {}
Nulls::~Nulls() noexcept {}
Number::~Number() noexcept {}
Operand::~Operand() noexcept {}
Operands::~Operands() noexcept {}
Order::~Order() noexcept {}
OrderByClause::~OrderByClause() noexcept {}
Orders::~Orders() noexcept {}
PrimaryKey::~PrimaryKey() noexcept {}
RowValueConstructor::~RowValueConstructor() noexcept {}
SchemaName::~SchemaName() noexcept {}
SelectExpression::~SelectExpression() noexcept {}
SelectExpressions::~SelectExpressions() noexcept {}
SelectModifier::~SelectModifier() noexcept {}
Summand::~Summand() noexcept {}
TableAlias::~TableAlias() noexcept {}
TableExpression::~TableExpression() noexcept {}
Term::~Term() noexcept {}
Terms::~Terms() noexcept {}
Value::~Value() noexcept {}
WhenClause::~WhenClause() noexcept {}
WhenClauses::~WhenClauses() noexcept {}
WhereClause::~WhereClause() noexcept {}


QuerySelect::QuerySelect( std::shared_ptr< SelectModifier > const& arg1, std::shared_ptr< SelectExpressions > const& arg2, std::shared_ptr< FromClause > const& arg3, std::shared_ptr< WhereClause > const& arg4, std::shared_ptr< GroupByClause > const& arg5, std::shared_ptr< HavingClause > const& arg6, std::shared_ptr< OrderByClause > const& arg7, std::shared_ptr< LimitClause > const& arg8 )
  :std::tuple< std::shared_ptr< SelectModifier >, std::shared_ptr< SelectExpressions >, std::shared_ptr< FromClause >, std::shared_ptr< WhereClause >, std::shared_ptr< GroupByClause >, std::shared_ptr< HavingClause >, std::shared_ptr< OrderByClause >, std::shared_ptr< LimitClause > >( arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 ) {}

void QuerySelect::accept( Visitor& visitor ) {
  visitor.visitQuerySelect( *this );
}
void QuerySelect::accept( ConstVisitor& visitor ) const {
  visitor.visitQuerySelect( *this );
}


AndConditionCondition::AndConditionCondition( std::shared_ptr< Condition > const& arg1 )
  :std::tuple< std::shared_ptr< Condition > >( arg1 ) {}

void AndConditionCondition::accept( Visitor& visitor ) {
  visitor.visitAndConditionCondition( *this );
}
void AndConditionCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitAndConditionCondition( *this );
}


AndConditionAnd::AndConditionAnd( std::shared_ptr< Condition > const& arg1, std::shared_ptr< AndCondition > const& arg2 )
  :std::tuple< std::shared_ptr< Condition >, std::shared_ptr< AndCondition > >( arg1, arg2 ) {}

void AndConditionAnd::accept( Visitor& visitor ) {
  visitor.visitAndConditionAnd( *this );
}
void AndConditionAnd::accept( ConstVisitor& visitor ) const {
  visitor.visitAndConditionAnd( *this );
}


AsEps::AsEps(  )
  :std::tuple<  >(  ) {}

void AsEps::accept( Visitor& visitor ) {
  visitor.visitAsEps( *this );
}
void AsEps::accept( ConstVisitor& visitor ) const {
  visitor.visitAsEps( *this );
}


AsAs::AsAs(  )
  :std::tuple<  >(  ) {}

void AsAs::accept( Visitor& visitor ) {
  visitor.visitAsAs( *this );
}
void AsAs::accept( ConstVisitor& visitor ) const {
  visitor.visitAsAs( *this );
}


AscDescEps::AscDescEps(  )
  :std::tuple<  >(  ) {}

void AscDescEps::accept( Visitor& visitor ) {
  visitor.visitAscDescEps( *this );
}
void AscDescEps::accept( ConstVisitor& visitor ) const {
  visitor.visitAscDescEps( *this );
}


AscDescAsc::AscDescAsc(  )
  :std::tuple<  >(  ) {}

void AscDescAsc::accept( Visitor& visitor ) {
  visitor.visitAscDescAsc( *this );
}
void AscDescAsc::accept( ConstVisitor& visitor ) const {
  visitor.visitAscDescAsc( *this );
}


AscDescDesc::AscDescDesc(  )
  :std::tuple<  >(  ) {}

void AscDescDesc::accept( Visitor& visitor ) {
  visitor.visitAscDescDesc( *this );
}
void AscDescDesc::accept( ConstVisitor& visitor ) const {
  visitor.visitAscDescDesc( *this );
}


BetweenConditionBetween::BetweenConditionBetween( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operand > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operand > >( arg1, arg2 ) {}

void BetweenConditionBetween::accept( Visitor& visitor ) {
  visitor.visitBetweenConditionBetween( *this );
}
void BetweenConditionBetween::accept( ConstVisitor& visitor ) const {
  visitor.visitBetweenConditionBetween( *this );
}


BetweenConditionNotBetween::BetweenConditionNotBetween( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operand > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operand > >( arg1, arg2 ) {}

void BetweenConditionNotBetween::accept( Visitor& visitor ) {
  visitor.visitBetweenConditionNotBetween( *this );
}
void BetweenConditionNotBetween::accept( ConstVisitor& visitor ) const {
  visitor.visitBetweenConditionNotBetween( *this );
}


BindParameterQuestion::BindParameterQuestion(  )
  :std::tuple<  >(  ) {}

void BindParameterQuestion::accept( Visitor& visitor ) {
  visitor.visitBindParameterQuestion( *this );
}
void BindParameterQuestion::accept( ConstVisitor& visitor ) const {
  visitor.visitBindParameterQuestion( *this );
}


BindParameterRef::BindParameterRef( int const& arg1 )
  :std::tuple< int >( arg1 ) {}

void BindParameterRef::accept( Visitor& visitor ) {
  visitor.visitBindParameterRef( *this );
}
void BindParameterRef::accept( ConstVisitor& visitor ) const {
  visitor.visitBindParameterRef( *this );
}


Case::Case( std::shared_ptr< Term > const& arg1, std::shared_ptr< WhenClauses > const& arg2, std::shared_ptr< ElseClause > const& arg3 )
  :std::tuple< std::shared_ptr< Term >, std::shared_ptr< WhenClauses >, std::shared_ptr< ElseClause > >( arg1, arg2, arg3 ) {}

void Case::accept( Visitor& visitor ) {
  visitor.visitCase( *this );
}
void Case::accept( ConstVisitor& visitor ) const {
  visitor.visitCase( *this );
}


CaseWhen::CaseWhen( std::shared_ptr< WhenClauses > const& arg1, std::shared_ptr< ElseClause > const& arg2 )
  :std::tuple< std::shared_ptr< WhenClauses >, std::shared_ptr< ElseClause > >( arg1, arg2 ) {}

void CaseWhen::accept( Visitor& visitor ) {
  visitor.visitCaseWhen( *this );
}
void CaseWhen::accept( ConstVisitor& visitor ) const {
  visitor.visitCaseWhen( *this );
}


ColumnDefinition::ColumnDefinition( std::shared_ptr< ColumnRef > const& arg1, std::shared_ptr< DataType > const& arg2, std::shared_ptr< Nullability > const& arg3, std::shared_ptr< PrimaryKey > const& arg4 )
  :std::tuple< std::shared_ptr< ColumnRef >, std::shared_ptr< DataType >, std::shared_ptr< Nullability >, std::shared_ptr< PrimaryKey > >( arg1, arg2, arg3, arg4 ) {}

void ColumnDefinition::accept( Visitor& visitor ) {
  visitor.visitColumnDefinition( *this );
}
void ColumnDefinition::accept( ConstVisitor& visitor ) const {
  visitor.visitColumnDefinition( *this );
}


ColumnDefsLast::ColumnDefsLast( std::shared_ptr< ColumnDef > const& arg1 )
  :std::tuple< std::shared_ptr< ColumnDef > >( arg1 ) {}

void ColumnDefsLast::accept( Visitor& visitor ) {
  visitor.visitColumnDefsLast( *this );
}
void ColumnDefsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitColumnDefsLast( *this );
}


ColumnDefsCons::ColumnDefsCons( std::shared_ptr< ColumnDef > const& arg1, std::shared_ptr< ColumnDefs > const& arg2 )
  :std::tuple< std::shared_ptr< ColumnDef >, std::shared_ptr< ColumnDefs > >( arg1, arg2 ) {}

void ColumnDefsCons::accept( Visitor& visitor ) {
  visitor.visitColumnDefsCons( *this );
}
void ColumnDefsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitColumnDefsCons( *this );
}


Column::Column( std::shared_ptr< FamilyName > const& arg1, std::string const& arg2 )
  :std::tuple< std::shared_ptr< FamilyName >, std::string >( arg1, arg2 ) {}

void Column::accept( Visitor& visitor ) {
  visitor.visitColumn( *this );
}
void Column::accept( ConstVisitor& visitor ) const {
  visitor.visitColumn( *this );
}


CompareNE::CompareNE(  )
  :std::tuple<  >(  ) {}

void CompareNE::accept( Visitor& visitor ) {
  visitor.visitCompareNE( *this );
}
void CompareNE::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareNE( *this );
}


CompareLE::CompareLE(  )
  :std::tuple<  >(  ) {}

void CompareLE::accept( Visitor& visitor ) {
  visitor.visitCompareLE( *this );
}
void CompareLE::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareLE( *this );
}


CompareGE::CompareGE(  )
  :std::tuple<  >(  ) {}

void CompareGE::accept( Visitor& visitor ) {
  visitor.visitCompareGE( *this );
}
void CompareGE::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareGE( *this );
}


CompareEQ::CompareEQ(  )
  :std::tuple<  >(  ) {}

void CompareEQ::accept( Visitor& visitor ) {
  visitor.visitCompareEQ( *this );
}
void CompareEQ::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareEQ( *this );
}


CompareLT::CompareLT(  )
  :std::tuple<  >(  ) {}

void CompareLT::accept( Visitor& visitor ) {
  visitor.visitCompareLT( *this );
}
void CompareLT::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareLT( *this );
}


CompareGT::CompareGT(  )
  :std::tuple<  >(  ) {}

void CompareGT::accept( Visitor& visitor ) {
  visitor.visitCompareGT( *this );
}
void CompareGT::accept( ConstVisitor& visitor ) const {
  visitor.visitCompareGT( *this );
}


ConditionCompare::ConditionCompare( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Compare > const& arg2, std::shared_ptr< Operand > const& arg3 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Compare >, std::shared_ptr< Operand > >( arg1, arg2, arg3 ) {}

void ConditionCompare::accept( Visitor& visitor ) {
  visitor.visitConditionCompare( *this );
}
void ConditionCompare::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionCompare( *this );
}


ConditionInCondition::ConditionInCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< InCondition > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< InCondition > >( arg1, arg2 ) {}

void ConditionInCondition::accept( Visitor& visitor ) {
  visitor.visitConditionInCondition( *this );
}
void ConditionInCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionInCondition( *this );
}


ConditionLikeCondition::ConditionLikeCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< LikeCondition > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< LikeCondition > >( arg1, arg2 ) {}

void ConditionLikeCondition::accept( Visitor& visitor ) {
  visitor.visitConditionLikeCondition( *this );
}
void ConditionLikeCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionLikeCondition( *this );
}


ConditionBetweenCondition::ConditionBetweenCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< BetweenCondition > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< BetweenCondition > >( arg1, arg2 ) {}

void ConditionBetweenCondition::accept( Visitor& visitor ) {
  visitor.visitConditionBetweenCondition( *this );
}
void ConditionBetweenCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionBetweenCondition( *this );
}


ConditionIsNullCondition::ConditionIsNullCondition( std::shared_ptr< Operand > const& arg1, std::shared_ptr< IsNullCondition > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< IsNullCondition > >( arg1, arg2 ) {}

void ConditionIsNullCondition::accept( Visitor& visitor ) {
  visitor.visitConditionIsNullCondition( *this );
}
void ConditionIsNullCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionIsNullCondition( *this );
}


ConditionNotExpression::ConditionNotExpression( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void ConditionNotExpression::accept( Visitor& visitor ) {
  visitor.visitConditionNotExpression( *this );
}
void ConditionNotExpression::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionNotExpression( *this );
}


ConditionParen::ConditionParen( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void ConditionParen::accept( Visitor& visitor ) {
  visitor.visitConditionParen( *this );
}
void ConditionParen::accept( ConstVisitor& visitor ) const {
  visitor.visitConditionParen( *this );
}


CharType::CharType( int const& arg1 )
  :std::tuple< int >( arg1 ) {}

void CharType::accept( Visitor& visitor ) {
  visitor.visitCharType( *this );
}
void CharType::accept( ConstVisitor& visitor ) const {
  visitor.visitCharType( *this );
}


IntegerType::IntegerType(  )
  :std::tuple<  >(  ) {}

void IntegerType::accept( Visitor& visitor ) {
  visitor.visitIntegerType( *this );
}
void IntegerType::accept( ConstVisitor& visitor ) const {
  visitor.visitIntegerType( *this );
}


BooleanType::BooleanType(  )
  :std::tuple<  >(  ) {}

void BooleanType::accept( Visitor& visitor ) {
  visitor.visitBooleanType( *this );
}
void BooleanType::accept( ConstVisitor& visitor ) const {
  visitor.visitBooleanType( *this );
}


FloatType::FloatType(  )
  :std::tuple<  >(  ) {}

void FloatType::accept( Visitor& visitor ) {
  visitor.visitFloatType( *this );
}
void FloatType::accept( ConstVisitor& visitor ) const {
  visitor.visitFloatType( *this );
}


DateType::DateType(  )
  :std::tuple<  >(  ) {}

void DateType::accept( Visitor& visitor ) {
  visitor.visitDateType( *this );
}
void DateType::accept( ConstVisitor& visitor ) const {
  visitor.visitDateType( *this );
}


ElseClauseEps::ElseClauseEps(  )
  :std::tuple<  >(  ) {}

void ElseClauseEps::accept( Visitor& visitor ) {
  visitor.visitElseClauseEps( *this );
}
void ElseClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitElseClauseEps( *this );
}


ElseClauseElse::ElseClauseElse( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void ElseClauseElse::accept( Visitor& visitor ) {
  visitor.visitElseClauseElse( *this );
}
void ElseClauseElse::accept( ConstVisitor& visitor ) const {
  visitor.visitElseClauseElse( *this );
}


ExpressionAndCondition::ExpressionAndCondition( std::shared_ptr< AndCondition > const& arg1 )
  :std::tuple< std::shared_ptr< AndCondition > >( arg1 ) {}

void ExpressionAndCondition::accept( Visitor& visitor ) {
  visitor.visitExpressionAndCondition( *this );
}
void ExpressionAndCondition::accept( ConstVisitor& visitor ) const {
  visitor.visitExpressionAndCondition( *this );
}


ExpressionOr::ExpressionOr( std::shared_ptr< AndCondition > const& arg1, std::shared_ptr< Expression > const& arg2 )
  :std::tuple< std::shared_ptr< AndCondition >, std::shared_ptr< Expression > >( arg1, arg2 ) {}

void ExpressionOr::accept( Visitor& visitor ) {
  visitor.visitExpressionOr( *this );
}
void ExpressionOr::accept( ConstVisitor& visitor ) const {
  visitor.visitExpressionOr( *this );
}


ExpressionsLast::ExpressionsLast( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void ExpressionsLast::accept( Visitor& visitor ) {
  visitor.visitExpressionsLast( *this );
}
void ExpressionsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitExpressionsLast( *this );
}


ExpressionsCons::ExpressionsCons( std::shared_ptr< Expression > const& arg1, std::shared_ptr< Expressions > const& arg2 )
  :std::tuple< std::shared_ptr< Expression >, std::shared_ptr< Expressions > >( arg1, arg2 ) {}

void ExpressionsCons::accept( Visitor& visitor ) {
  visitor.visitExpressionsCons( *this );
}
void ExpressionsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitExpressionsCons( *this );
}


FactorTerm::FactorTerm( std::shared_ptr< Term > const& arg1 )
  :std::tuple< std::shared_ptr< Term > >( arg1 ) {}

void FactorTerm::accept( Visitor& visitor ) {
  visitor.visitFactorTerm( *this );
}
void FactorTerm::accept( ConstVisitor& visitor ) const {
  visitor.visitFactorTerm( *this );
}


FactorTimes::FactorTimes( std::shared_ptr< Factor > const& arg1, std::shared_ptr< Term > const& arg2 )
  :std::tuple< std::shared_ptr< Factor >, std::shared_ptr< Term > >( arg1, arg2 ) {}

void FactorTimes::accept( Visitor& visitor ) {
  visitor.visitFactorTimes( *this );
}
void FactorTimes::accept( ConstVisitor& visitor ) const {
  visitor.visitFactorTimes( *this );
}


FactorDivides::FactorDivides( std::shared_ptr< Factor > const& arg1, std::shared_ptr< Term > const& arg2 )
  :std::tuple< std::shared_ptr< Factor >, std::shared_ptr< Term > >( arg1, arg2 ) {}

void FactorDivides::accept( Visitor& visitor ) {
  visitor.visitFactorDivides( *this );
}
void FactorDivides::accept( ConstVisitor& visitor ) const {
  visitor.visitFactorDivides( *this );
}


FamilyNameEps::FamilyNameEps(  )
  :std::tuple<  >(  ) {}

void FamilyNameEps::accept( Visitor& visitor ) {
  visitor.visitFamilyNameEps( *this );
}
void FamilyNameEps::accept( ConstVisitor& visitor ) const {
  visitor.visitFamilyNameEps( *this );
}


FamilyNameFamily::FamilyNameFamily( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void FamilyNameFamily::accept( Visitor& visitor ) {
  visitor.visitFamilyNameFamily( *this );
}
void FamilyNameFamily::accept( ConstVisitor& visitor ) const {
  visitor.visitFamilyNameFamily( *this );
}


From::From( std::shared_ptr< TableExpression > const& arg1 )
  :std::tuple< std::shared_ptr< TableExpression > >( arg1 ) {}

void From::accept( Visitor& visitor ) {
  visitor.visitFrom( *this );
}
void From::accept( ConstVisitor& visitor ) const {
  visitor.visitFrom( *this );
}


FromWithColumnDefs::FromWithColumnDefs( std::shared_ptr< TableExpression > const& arg1, std::shared_ptr< ColumnDefs > const& arg2 )
  :std::tuple< std::shared_ptr< TableExpression >, std::shared_ptr< ColumnDefs > >( arg1, arg2 ) {}

void FromWithColumnDefs::accept( Visitor& visitor ) {
  visitor.visitFromWithColumnDefs( *this );
}
void FromWithColumnDefs::accept( ConstVisitor& visitor ) const {
  visitor.visitFromWithColumnDefs( *this );
}


GroupByClauseEps::GroupByClauseEps(  )
  :std::tuple<  >(  ) {}

void GroupByClauseEps::accept( Visitor& visitor ) {
  visitor.visitGroupByClauseEps( *this );
}
void GroupByClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitGroupByClauseEps( *this );
}


GroupByClauseGroupBy::GroupByClauseGroupBy( std::shared_ptr< Expressions > const& arg1 )
  :std::tuple< std::shared_ptr< Expressions > >( arg1 ) {}

void GroupByClauseGroupBy::accept( Visitor& visitor ) {
  visitor.visitGroupByClauseGroupBy( *this );
}
void GroupByClauseGroupBy::accept( ConstVisitor& visitor ) const {
  visitor.visitGroupByClauseGroupBy( *this );
}


HavingClauseEps::HavingClauseEps(  )
  :std::tuple<  >(  ) {}

void HavingClauseEps::accept( Visitor& visitor ) {
  visitor.visitHavingClauseEps( *this );
}
void HavingClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitHavingClauseEps( *this );
}


HavingClauseHaving::HavingClauseHaving( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void HavingClauseHaving::accept( Visitor& visitor ) {
  visitor.visitHavingClauseHaving( *this );
}
void HavingClauseHaving::accept( ConstVisitor& visitor ) const {
  visitor.visitHavingClauseHaving( *this );
}


InConditionIn::InConditionIn( std::shared_ptr< Operands > const& arg1 )
  :std::tuple< std::shared_ptr< Operands > >( arg1 ) {}

void InConditionIn::accept( Visitor& visitor ) {
  visitor.visitInConditionIn( *this );
}
void InConditionIn::accept( ConstVisitor& visitor ) const {
  visitor.visitInConditionIn( *this );
}


InConditionNotIn::InConditionNotIn( std::shared_ptr< Operands > const& arg1 )
  :std::tuple< std::shared_ptr< Operands > >( arg1 ) {}

void InConditionNotIn::accept( Visitor& visitor ) {
  visitor.visitInConditionNotIn( *this );
}
void InConditionNotIn::accept( ConstVisitor& visitor ) const {
  visitor.visitInConditionNotIn( *this );
}


IsNullConditionIsNull::IsNullConditionIsNull(  )
  :std::tuple<  >(  ) {}

void IsNullConditionIsNull::accept( Visitor& visitor ) {
  visitor.visitIsNullConditionIsNull( *this );
}
void IsNullConditionIsNull::accept( ConstVisitor& visitor ) const {
  visitor.visitIsNullConditionIsNull( *this );
}


IsNullConditionIsNotNull::IsNullConditionIsNotNull(  )
  :std::tuple<  >(  ) {}

void IsNullConditionIsNotNull::accept( Visitor& visitor ) {
  visitor.visitIsNullConditionIsNotNull( *this );
}
void IsNullConditionIsNotNull::accept( ConstVisitor& visitor ) const {
  visitor.visitIsNullConditionIsNotNull( *this );
}


LikeConditionLike::LikeConditionLike( std::shared_ptr< Operand > const& arg1 )
  :std::tuple< std::shared_ptr< Operand > >( arg1 ) {}

void LikeConditionLike::accept( Visitor& visitor ) {
  visitor.visitLikeConditionLike( *this );
}
void LikeConditionLike::accept( ConstVisitor& visitor ) const {
  visitor.visitLikeConditionLike( *this );
}


LikeConditionNotLike::LikeConditionNotLike( std::shared_ptr< Operand > const& arg1 )
  :std::tuple< std::shared_ptr< Operand > >( arg1 ) {}

void LikeConditionNotLike::accept( Visitor& visitor ) {
  visitor.visitLikeConditionNotLike( *this );
}
void LikeConditionNotLike::accept( ConstVisitor& visitor ) const {
  visitor.visitLikeConditionNotLike( *this );
}


LimitClauseEps::LimitClauseEps(  )
  :std::tuple<  >(  ) {}

void LimitClauseEps::accept( Visitor& visitor ) {
  visitor.visitLimitClauseEps( *this );
}
void LimitClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitLimitClauseEps( *this );
}


LimitClauseLimitBindParameter::LimitClauseLimitBindParameter( std::shared_ptr< BindParameter > const& arg1 )
  :std::tuple< std::shared_ptr< BindParameter > >( arg1 ) {}

void LimitClauseLimitBindParameter::accept( Visitor& visitor ) {
  visitor.visitLimitClauseLimitBindParameter( *this );
}
void LimitClauseLimitBindParameter::accept( ConstVisitor& visitor ) const {
  visitor.visitLimitClauseLimitBindParameter( *this );
}


LimitClauseLimitNumber::LimitClauseLimitNumber( std::shared_ptr< Number > const& arg1 )
  :std::tuple< std::shared_ptr< Number > >( arg1 ) {}

void LimitClauseLimitNumber::accept( Visitor& visitor ) {
  visitor.visitLimitClauseLimitNumber( *this );
}
void LimitClauseLimitNumber::accept( ConstVisitor& visitor ) const {
  visitor.visitLimitClauseLimitNumber( *this );
}


NullabilityEps::NullabilityEps(  )
  :std::tuple<  >(  ) {}

void NullabilityEps::accept( Visitor& visitor ) {
  visitor.visitNullabilityEps( *this );
}
void NullabilityEps::accept( ConstVisitor& visitor ) const {
  visitor.visitNullabilityEps( *this );
}


NullabilityNull::NullabilityNull(  )
  :std::tuple<  >(  ) {}

void NullabilityNull::accept( Visitor& visitor ) {
  visitor.visitNullabilityNull( *this );
}
void NullabilityNull::accept( ConstVisitor& visitor ) const {
  visitor.visitNullabilityNull( *this );
}


NullabilityNotNull::NullabilityNotNull(  )
  :std::tuple<  >(  ) {}

void NullabilityNotNull::accept( Visitor& visitor ) {
  visitor.visitNullabilityNotNull( *this );
}
void NullabilityNotNull::accept( ConstVisitor& visitor ) const {
  visitor.visitNullabilityNotNull( *this );
}


NullsEps::NullsEps(  )
  :std::tuple<  >(  ) {}

void NullsEps::accept( Visitor& visitor ) {
  visitor.visitNullsEps( *this );
}
void NullsEps::accept( ConstVisitor& visitor ) const {
  visitor.visitNullsEps( *this );
}


NullsFirst::NullsFirst(  )
  :std::tuple<  >(  ) {}

void NullsFirst::accept( Visitor& visitor ) {
  visitor.visitNullsFirst( *this );
}
void NullsFirst::accept( ConstVisitor& visitor ) const {
  visitor.visitNullsFirst( *this );
}


NullsLast::NullsLast(  )
  :std::tuple<  >(  ) {}

void NullsLast::accept( Visitor& visitor ) {
  visitor.visitNullsLast( *this );
}
void NullsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitNullsLast( *this );
}


NumberInteger::NumberInteger( int const& arg1 )
  :std::tuple< int >( arg1 ) {}

void NumberInteger::accept( Visitor& visitor ) {
  visitor.visitNumberInteger( *this );
}
void NumberInteger::accept( ConstVisitor& visitor ) const {
  visitor.visitNumberInteger( *this );
}


OperandSummand::OperandSummand( std::shared_ptr< Summand > const& arg1 )
  :std::tuple< std::shared_ptr< Summand > >( arg1 ) {}

void OperandSummand::accept( Visitor& visitor ) {
  visitor.visitOperandSummand( *this );
}
void OperandSummand::accept( ConstVisitor& visitor ) const {
  visitor.visitOperandSummand( *this );
}


OperandAppend::OperandAppend( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Summand > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Summand > >( arg1, arg2 ) {}

void OperandAppend::accept( Visitor& visitor ) {
  visitor.visitOperandAppend( *this );
}
void OperandAppend::accept( ConstVisitor& visitor ) const {
  visitor.visitOperandAppend( *this );
}


OperandsLast::OperandsLast( std::shared_ptr< Operand > const& arg1 )
  :std::tuple< std::shared_ptr< Operand > >( arg1 ) {}

void OperandsLast::accept( Visitor& visitor ) {
  visitor.visitOperandsLast( *this );
}
void OperandsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitOperandsLast( *this );
}


OperandsCons::OperandsCons( std::shared_ptr< Operand > const& arg1, std::shared_ptr< Operands > const& arg2 )
  :std::tuple< std::shared_ptr< Operand >, std::shared_ptr< Operands > >( arg1, arg2 ) {}

void OperandsCons::accept( Visitor& visitor ) {
  visitor.visitOperandsCons( *this );
}
void OperandsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitOperandsCons( *this );
}


OrderOrder::OrderOrder( std::shared_ptr< Expression > const& arg1, std::shared_ptr< AscDesc > const& arg2, std::shared_ptr< Nulls > const& arg3 )
  :std::tuple< std::shared_ptr< Expression >, std::shared_ptr< AscDesc >, std::shared_ptr< Nulls > >( arg1, arg2, arg3 ) {}

void OrderOrder::accept( Visitor& visitor ) {
  visitor.visitOrderOrder( *this );
}
void OrderOrder::accept( ConstVisitor& visitor ) const {
  visitor.visitOrderOrder( *this );
}


OrderByClauseEps::OrderByClauseEps(  )
  :std::tuple<  >(  ) {}

void OrderByClauseEps::accept( Visitor& visitor ) {
  visitor.visitOrderByClauseEps( *this );
}
void OrderByClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitOrderByClauseEps( *this );
}


OrderByClauseOrderBy::OrderByClauseOrderBy( std::shared_ptr< Orders > const& arg1 )
  :std::tuple< std::shared_ptr< Orders > >( arg1 ) {}

void OrderByClauseOrderBy::accept( Visitor& visitor ) {
  visitor.visitOrderByClauseOrderBy( *this );
}
void OrderByClauseOrderBy::accept( ConstVisitor& visitor ) const {
  visitor.visitOrderByClauseOrderBy( *this );
}


OrdersLast::OrdersLast( std::shared_ptr< Order > const& arg1 )
  :std::tuple< std::shared_ptr< Order > >( arg1 ) {}

void OrdersLast::accept( Visitor& visitor ) {
  visitor.visitOrdersLast( *this );
}
void OrdersLast::accept( ConstVisitor& visitor ) const {
  visitor.visitOrdersLast( *this );
}


OrdersCons::OrdersCons( std::shared_ptr< Order > const& arg1, std::shared_ptr< Orders > const& arg2 )
  :std::tuple< std::shared_ptr< Order >, std::shared_ptr< Orders > >( arg1, arg2 ) {}

void OrdersCons::accept( Visitor& visitor ) {
  visitor.visitOrdersCons( *this );
}
void OrdersCons::accept( ConstVisitor& visitor ) const {
  visitor.visitOrdersCons( *this );
}


PrimaryKeyEps::PrimaryKeyEps(  )
  :std::tuple<  >(  ) {}

void PrimaryKeyEps::accept( Visitor& visitor ) {
  visitor.visitPrimaryKeyEps( *this );
}
void PrimaryKeyEps::accept( ConstVisitor& visitor ) const {
  visitor.visitPrimaryKeyEps( *this );
}


PrimaryKeyPrimaryKey::PrimaryKeyPrimaryKey( std::shared_ptr< AscDesc > const& arg1 )
  :std::tuple< std::shared_ptr< AscDesc > >( arg1 ) {}

void PrimaryKeyPrimaryKey::accept( Visitor& visitor ) {
  visitor.visitPrimaryKeyPrimaryKey( *this );
}
void PrimaryKeyPrimaryKey::accept( ConstVisitor& visitor ) const {
  visitor.visitPrimaryKeyPrimaryKey( *this );
}


RowValue::RowValue( std::shared_ptr< Term > const& arg1, std::shared_ptr< Terms > const& arg2 )
  :std::tuple< std::shared_ptr< Term >, std::shared_ptr< Terms > >( arg1, arg2 ) {}

void RowValue::accept( Visitor& visitor ) {
  visitor.visitRowValue( *this );
}
void RowValue::accept( ConstVisitor& visitor ) const {
  visitor.visitRowValue( *this );
}


SchemaNameEps::SchemaNameEps(  )
  :std::tuple<  >(  ) {}

void SchemaNameEps::accept( Visitor& visitor ) {
  visitor.visitSchemaNameEps( *this );
}
void SchemaNameEps::accept( ConstVisitor& visitor ) const {
  visitor.visitSchemaNameEps( *this );
}


SchemaNameSchema::SchemaNameSchema( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void SchemaNameSchema::accept( Visitor& visitor ) {
  visitor.visitSchemaNameSchema( *this );
}
void SchemaNameSchema::accept( ConstVisitor& visitor ) const {
  visitor.visitSchemaNameSchema( *this );
}


SelectExpressionAny::SelectExpressionAny( std::shared_ptr< FamilyName > const& arg1 )
  :std::tuple< std::shared_ptr< FamilyName > >( arg1 ) {}

void SelectExpressionAny::accept( Visitor& visitor ) {
  visitor.visitSelectExpressionAny( *this );
}
void SelectExpressionAny::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectExpressionAny( *this );
}


SelectExpressionTerm::SelectExpressionTerm( std::shared_ptr< Term > const& arg1 )
  :std::tuple< std::shared_ptr< Term > >( arg1 ) {}

void SelectExpressionTerm::accept( Visitor& visitor ) {
  visitor.visitSelectExpressionTerm( *this );
}
void SelectExpressionTerm::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectExpressionTerm( *this );
}


SelectExpressionTermAs::SelectExpressionTermAs( std::shared_ptr< Term > const& arg1, std::shared_ptr< As > const& arg2, std::string const& arg3 )
  :std::tuple< std::shared_ptr< Term >, std::shared_ptr< As >, std::string >( arg1, arg2, arg3 ) {}

void SelectExpressionTermAs::accept( Visitor& visitor ) {
  visitor.visitSelectExpressionTermAs( *this );
}
void SelectExpressionTermAs::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectExpressionTermAs( *this );
}


SelectExpressionsLast::SelectExpressionsLast( std::shared_ptr< SelectExpression > const& arg1 )
  :std::tuple< std::shared_ptr< SelectExpression > >( arg1 ) {}

void SelectExpressionsLast::accept( Visitor& visitor ) {
  visitor.visitSelectExpressionsLast( *this );
}
void SelectExpressionsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectExpressionsLast( *this );
}


SelectExpressionsCons::SelectExpressionsCons( std::shared_ptr< SelectExpression > const& arg1, std::shared_ptr< SelectExpressions > const& arg2 )
  :std::tuple< std::shared_ptr< SelectExpression >, std::shared_ptr< SelectExpressions > >( arg1, arg2 ) {}

void SelectExpressionsCons::accept( Visitor& visitor ) {
  visitor.visitSelectExpressionsCons( *this );
}
void SelectExpressionsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectExpressionsCons( *this );
}


SelectModifierEps::SelectModifierEps(  )
  :std::tuple<  >(  ) {}

void SelectModifierEps::accept( Visitor& visitor ) {
  visitor.visitSelectModifierEps( *this );
}
void SelectModifierEps::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectModifierEps( *this );
}


SelectModifierDistinct::SelectModifierDistinct(  )
  :std::tuple<  >(  ) {}

void SelectModifierDistinct::accept( Visitor& visitor ) {
  visitor.visitSelectModifierDistinct( *this );
}
void SelectModifierDistinct::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectModifierDistinct( *this );
}


SelectModifierAll::SelectModifierAll(  )
  :std::tuple<  >(  ) {}

void SelectModifierAll::accept( Visitor& visitor ) {
  visitor.visitSelectModifierAll( *this );
}
void SelectModifierAll::accept( ConstVisitor& visitor ) const {
  visitor.visitSelectModifierAll( *this );
}


SummandFactor::SummandFactor( std::shared_ptr< Factor > const& arg1 )
  :std::tuple< std::shared_ptr< Factor > >( arg1 ) {}

void SummandFactor::accept( Visitor& visitor ) {
  visitor.visitSummandFactor( *this );
}
void SummandFactor::accept( ConstVisitor& visitor ) const {
  visitor.visitSummandFactor( *this );
}


SummandPlus::SummandPlus( std::shared_ptr< Summand > const& arg1, std::shared_ptr< Factor > const& arg2 )
  :std::tuple< std::shared_ptr< Summand >, std::shared_ptr< Factor > >( arg1, arg2 ) {}

void SummandPlus::accept( Visitor& visitor ) {
  visitor.visitSummandPlus( *this );
}
void SummandPlus::accept( ConstVisitor& visitor ) const {
  visitor.visitSummandPlus( *this );
}


SummandMinus::SummandMinus( std::shared_ptr< Summand > const& arg1, std::shared_ptr< Factor > const& arg2 )
  :std::tuple< std::shared_ptr< Summand >, std::shared_ptr< Factor > >( arg1, arg2 ) {}

void SummandMinus::accept( Visitor& visitor ) {
  visitor.visitSummandMinus( *this );
}
void SummandMinus::accept( ConstVisitor& visitor ) const {
  visitor.visitSummandMinus( *this );
}


TableAliasEps::TableAliasEps(  )
  :std::tuple<  >(  ) {}

void TableAliasEps::accept( Visitor& visitor ) {
  visitor.visitTableAliasEps( *this );
}
void TableAliasEps::accept( ConstVisitor& visitor ) const {
  visitor.visitTableAliasEps( *this );
}


TableAliasAlias::TableAliasAlias( std::shared_ptr< As > const& arg1, std::string const& arg2 )
  :std::tuple< std::shared_ptr< As >, std::string >( arg1, arg2 ) {}

void TableAliasAlias::accept( Visitor& visitor ) {
  visitor.visitTableAliasAlias( *this );
}
void TableAliasAlias::accept( ConstVisitor& visitor ) const {
  visitor.visitTableAliasAlias( *this );
}


Table::Table( std::shared_ptr< SchemaName > const& arg1, std::string const& arg2, std::shared_ptr< TableAlias > const& arg3 )
  :std::tuple< std::shared_ptr< SchemaName >, std::string, std::shared_ptr< TableAlias > >( arg1, arg2, arg3 ) {}

void Table::accept( Visitor& visitor ) {
  visitor.visitTable( *this );
}
void Table::accept( ConstVisitor& visitor ) const {
  visitor.visitTable( *this );
}


TermValue::TermValue( std::shared_ptr< Value > const& arg1 )
  :std::tuple< std::shared_ptr< Value > >( arg1 ) {}

void TermValue::accept( Visitor& visitor ) {
  visitor.visitTermValue( *this );
}
void TermValue::accept( ConstVisitor& visitor ) const {
  visitor.visitTermValue( *this );
}


TermBindParameter::TermBindParameter( std::shared_ptr< BindParameter > const& arg1 )
  :std::tuple< std::shared_ptr< BindParameter > >( arg1 ) {}

void TermBindParameter::accept( Visitor& visitor ) {
  visitor.visitTermBindParameter( *this );
}
void TermBindParameter::accept( ConstVisitor& visitor ) const {
  visitor.visitTermBindParameter( *this );
}


TermCase::TermCase( std::shared_ptr< CaseClause > const& arg1 )
  :std::tuple< std::shared_ptr< CaseClause > >( arg1 ) {}

void TermCase::accept( Visitor& visitor ) {
  visitor.visitTermCase( *this );
}
void TermCase::accept( ConstVisitor& visitor ) const {
  visitor.visitTermCase( *this );
}


TermCaseWhen::TermCaseWhen( std::shared_ptr< CaseWhenClause > const& arg1 )
  :std::tuple< std::shared_ptr< CaseWhenClause > >( arg1 ) {}

void TermCaseWhen::accept( Visitor& visitor ) {
  visitor.visitTermCaseWhen( *this );
}
void TermCaseWhen::accept( ConstVisitor& visitor ) const {
  visitor.visitTermCaseWhen( *this );
}


TermParen::TermParen( std::shared_ptr< Operand > const& arg1 )
  :std::tuple< std::shared_ptr< Operand > >( arg1 ) {}

void TermParen::accept( Visitor& visitor ) {
  visitor.visitTermParen( *this );
}
void TermParen::accept( ConstVisitor& visitor ) const {
  visitor.visitTermParen( *this );
}


TermColumnRef::TermColumnRef( std::shared_ptr< ColumnRef > const& arg1 )
  :std::tuple< std::shared_ptr< ColumnRef > >( arg1 ) {}

void TermColumnRef::accept( Visitor& visitor ) {
  visitor.visitTermColumnRef( *this );
}
void TermColumnRef::accept( ConstVisitor& visitor ) const {
  visitor.visitTermColumnRef( *this );
}


TermRowValueConstructor::TermRowValueConstructor( std::shared_ptr< RowValueConstructor > const& arg1 )
  :std::tuple< std::shared_ptr< RowValueConstructor > >( arg1 ) {}

void TermRowValueConstructor::accept( Visitor& visitor ) {
  visitor.visitTermRowValueConstructor( *this );
}
void TermRowValueConstructor::accept( ConstVisitor& visitor ) const {
  visitor.visitTermRowValueConstructor( *this );
}


TermsLast::TermsLast( std::shared_ptr< Term > const& arg1 )
  :std::tuple< std::shared_ptr< Term > >( arg1 ) {}

void TermsLast::accept( Visitor& visitor ) {
  visitor.visitTermsLast( *this );
}
void TermsLast::accept( ConstVisitor& visitor ) const {
  visitor.visitTermsLast( *this );
}


TermsCons::TermsCons( std::shared_ptr< Term > const& arg1, std::shared_ptr< Terms > const& arg2 )
  :std::tuple< std::shared_ptr< Term >, std::shared_ptr< Terms > >( arg1, arg2 ) {}

void TermsCons::accept( Visitor& visitor ) {
  visitor.visitTermsCons( *this );
}
void TermsCons::accept( ConstVisitor& visitor ) const {
  visitor.visitTermsCons( *this );
}


ValueString::ValueString( std::string const& arg1 )
  :std::tuple< std::string >( arg1 ) {}

void ValueString::accept( Visitor& visitor ) {
  visitor.visitValueString( *this );
}
void ValueString::accept( ConstVisitor& visitor ) const {
  visitor.visitValueString( *this );
}


ValueBoolean::ValueBoolean( bool const& arg1 )
  :std::tuple< bool >( arg1 ) {}

void ValueBoolean::accept( Visitor& visitor ) {
  visitor.visitValueBoolean( *this );
}
void ValueBoolean::accept( ConstVisitor& visitor ) const {
  visitor.visitValueBoolean( *this );
}


ValueTrue::ValueTrue(  )
  :std::tuple<  >(  ) {}

void ValueTrue::accept( Visitor& visitor ) {
  visitor.visitValueTrue( *this );
}
void ValueTrue::accept( ConstVisitor& visitor ) const {
  visitor.visitValueTrue( *this );
}


ValueFalse::ValueFalse(  )
  :std::tuple<  >(  ) {}

void ValueFalse::accept( Visitor& visitor ) {
  visitor.visitValueFalse( *this );
}
void ValueFalse::accept( ConstVisitor& visitor ) const {
  visitor.visitValueFalse( *this );
}


ValueNumber::ValueNumber( std::shared_ptr< Number > const& arg1 )
  :std::tuple< std::shared_ptr< Number > >( arg1 ) {}

void ValueNumber::accept( Visitor& visitor ) {
  visitor.visitValueNumber( *this );
}
void ValueNumber::accept( ConstVisitor& visitor ) const {
  visitor.visitValueNumber( *this );
}


ValueNull::ValueNull(  )
  :std::tuple<  >(  ) {}

void ValueNull::accept( Visitor& visitor ) {
  visitor.visitValueNull( *this );
}
void ValueNull::accept( ConstVisitor& visitor ) const {
  visitor.visitValueNull( *this );
}


When::When( std::shared_ptr< Expression > const& arg1, std::shared_ptr< Term > const& arg2 )
  :std::tuple< std::shared_ptr< Expression >, std::shared_ptr< Term > >( arg1, arg2 ) {}

void When::accept( Visitor& visitor ) {
  visitor.visitWhen( *this );
}
void When::accept( ConstVisitor& visitor ) const {
  visitor.visitWhen( *this );
}


WhenClausesLast::WhenClausesLast( std::shared_ptr< WhenClause > const& arg1 )
  :std::tuple< std::shared_ptr< WhenClause > >( arg1 ) {}

void WhenClausesLast::accept( Visitor& visitor ) {
  visitor.visitWhenClausesLast( *this );
}
void WhenClausesLast::accept( ConstVisitor& visitor ) const {
  visitor.visitWhenClausesLast( *this );
}


WhenClausesCons::WhenClausesCons( std::shared_ptr< WhenClause > const& arg1, std::shared_ptr< WhenClauses > const& arg2 )
  :std::tuple< std::shared_ptr< WhenClause >, std::shared_ptr< WhenClauses > >( arg1, arg2 ) {}

void WhenClausesCons::accept( Visitor& visitor ) {
  visitor.visitWhenClausesCons( *this );
}
void WhenClausesCons::accept( ConstVisitor& visitor ) const {
  visitor.visitWhenClausesCons( *this );
}


WhereClauseEps::WhereClauseEps(  )
  :std::tuple<  >(  ) {}

void WhereClauseEps::accept( Visitor& visitor ) {
  visitor.visitWhereClauseEps( *this );
}
void WhereClauseEps::accept( ConstVisitor& visitor ) const {
  visitor.visitWhereClauseEps( *this );
}


WhereClauseWhere::WhereClauseWhere( std::shared_ptr< Expression > const& arg1 )
  :std::tuple< std::shared_ptr< Expression > >( arg1 ) {}

void WhereClauseWhere::accept( Visitor& visitor ) {
  visitor.visitWhereClauseWhere( *this );
}
void WhereClauseWhere::accept( ConstVisitor& visitor ) const {
  visitor.visitWhereClauseWhere( *this );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::ostream& operator <<( std::ostream &out, Query const& self ) {
  class Visitor : public Query::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitQuerySelect( QuerySelect const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, AndCondition const& self ) {
  class Visitor : public AndCondition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAndConditionCondition( AndConditionCondition const& host ) {
      *out_ << host;
    }
    void visitAndConditionAnd( AndConditionAnd const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, As const& self ) {
  class Visitor : public As::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAsEps( AsEps const& host ) {
      *out_ << host;
    }
    void visitAsAs( AsAs const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, AscDesc const& self ) {
  class Visitor : public AscDesc::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitAscDescEps( AscDescEps const& host ) {
      *out_ << host;
    }
    void visitAscDescAsc( AscDescAsc const& host ) {
      *out_ << host;
    }
    void visitAscDescDesc( AscDescDesc const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, BetweenCondition const& self ) {
  class Visitor : public BetweenCondition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitBetweenConditionBetween( BetweenConditionBetween const& host ) {
      *out_ << host;
    }
    void visitBetweenConditionNotBetween( BetweenConditionNotBetween const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, BindParameter const& self ) {
  class Visitor : public BindParameter::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitBindParameterQuestion( BindParameterQuestion const& host ) {
      *out_ << host;
    }
    void visitBindParameterRef( BindParameterRef const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, CaseClause const& self ) {
  class Visitor : public CaseClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitCase( Case const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, CaseWhenClause const& self ) {
  class Visitor : public CaseWhenClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitCaseWhen( CaseWhen const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, ColumnDef const& self ) {
  class Visitor : public ColumnDef::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitColumnDefinition( ColumnDefinition const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, ColumnDefs const& self ) {
  class Visitor : public ColumnDefs::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitColumnDefsLast( ColumnDefsLast const& host ) {
      *out_ << host;
    }
    void visitColumnDefsCons( ColumnDefsCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, ColumnRef const& self ) {
  class Visitor : public ColumnRef::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitColumn( Column const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Compare const& self ) {
  class Visitor : public Compare::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitCompareNE( CompareNE const& host ) {
      *out_ << host;
    }
    void visitCompareLE( CompareLE const& host ) {
      *out_ << host;
    }
    void visitCompareGE( CompareGE const& host ) {
      *out_ << host;
    }
    void visitCompareEQ( CompareEQ const& host ) {
      *out_ << host;
    }
    void visitCompareLT( CompareLT const& host ) {
      *out_ << host;
    }
    void visitCompareGT( CompareGT const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Condition const& self ) {
  class Visitor : public Condition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitConditionCompare( ConditionCompare const& host ) {
      *out_ << host;
    }
    void visitConditionInCondition( ConditionInCondition const& host ) {
      *out_ << host;
    }
    void visitConditionLikeCondition( ConditionLikeCondition const& host ) {
      *out_ << host;
    }
    void visitConditionBetweenCondition( ConditionBetweenCondition const& host ) {
      *out_ << host;
    }
    void visitConditionIsNullCondition( ConditionIsNullCondition const& host ) {
      *out_ << host;
    }
    void visitConditionNotExpression( ConditionNotExpression const& host ) {
      *out_ << host;
    }
    void visitConditionParen( ConditionParen const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, DataType const& self ) {
  class Visitor : public DataType::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitCharType( CharType const& host ) {
      *out_ << host;
    }
    void visitIntegerType( IntegerType const& host ) {
      *out_ << host;
    }
    void visitBooleanType( BooleanType const& host ) {
      *out_ << host;
    }
    void visitFloatType( FloatType const& host ) {
      *out_ << host;
    }
    void visitDateType( DateType const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, ElseClause const& self ) {
  class Visitor : public ElseClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitElseClauseEps( ElseClauseEps const& host ) {
      *out_ << host;
    }
    void visitElseClauseElse( ElseClauseElse const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Expression const& self ) {
  class Visitor : public Expression::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitExpressionAndCondition( ExpressionAndCondition const& host ) {
      *out_ << host;
    }
    void visitExpressionOr( ExpressionOr const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Expressions const& self ) {
  class Visitor : public Expressions::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitExpressionsLast( ExpressionsLast const& host ) {
      *out_ << host;
    }
    void visitExpressionsCons( ExpressionsCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Factor const& self ) {
  class Visitor : public Factor::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitFactorTerm( FactorTerm const& host ) {
      *out_ << host;
    }
    void visitFactorTimes( FactorTimes const& host ) {
      *out_ << host;
    }
    void visitFactorDivides( FactorDivides const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, FamilyName const& self ) {
  class Visitor : public FamilyName::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitFamilyNameEps( FamilyNameEps const& host ) {
      *out_ << host;
    }
    void visitFamilyNameFamily( FamilyNameFamily const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, FromClause const& self ) {
  class Visitor : public FromClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitFrom( From const& host ) {
      *out_ << host;
    }
    void visitFromWithColumnDefs( FromWithColumnDefs const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, GroupByClause const& self ) {
  class Visitor : public GroupByClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitGroupByClauseEps( GroupByClauseEps const& host ) {
      *out_ << host;
    }
    void visitGroupByClauseGroupBy( GroupByClauseGroupBy const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, HavingClause const& self ) {
  class Visitor : public HavingClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitHavingClauseEps( HavingClauseEps const& host ) {
      *out_ << host;
    }
    void visitHavingClauseHaving( HavingClauseHaving const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, InCondition const& self ) {
  class Visitor : public InCondition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitInConditionIn( InConditionIn const& host ) {
      *out_ << host;
    }
    void visitInConditionNotIn( InConditionNotIn const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, IsNullCondition const& self ) {
  class Visitor : public IsNullCondition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitIsNullConditionIsNull( IsNullConditionIsNull const& host ) {
      *out_ << host;
    }
    void visitIsNullConditionIsNotNull( IsNullConditionIsNotNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, LikeCondition const& self ) {
  class Visitor : public LikeCondition::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitLikeConditionLike( LikeConditionLike const& host ) {
      *out_ << host;
    }
    void visitLikeConditionNotLike( LikeConditionNotLike const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, LimitClause const& self ) {
  class Visitor : public LimitClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitLimitClauseEps( LimitClauseEps const& host ) {
      *out_ << host;
    }
    void visitLimitClauseLimitBindParameter( LimitClauseLimitBindParameter const& host ) {
      *out_ << host;
    }
    void visitLimitClauseLimitNumber( LimitClauseLimitNumber const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Nullability const& self ) {
  class Visitor : public Nullability::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNullabilityEps( NullabilityEps const& host ) {
      *out_ << host;
    }
    void visitNullabilityNull( NullabilityNull const& host ) {
      *out_ << host;
    }
    void visitNullabilityNotNull( NullabilityNotNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Nulls const& self ) {
  class Visitor : public Nulls::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNullsEps( NullsEps const& host ) {
      *out_ << host;
    }
    void visitNullsFirst( NullsFirst const& host ) {
      *out_ << host;
    }
    void visitNullsLast( NullsLast const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Number const& self ) {
  class Visitor : public Number::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitNumberInteger( NumberInteger const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Operand const& self ) {
  class Visitor : public Operand::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitOperandSummand( OperandSummand const& host ) {
      *out_ << host;
    }
    void visitOperandAppend( OperandAppend const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Operands const& self ) {
  class Visitor : public Operands::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitOperandsLast( OperandsLast const& host ) {
      *out_ << host;
    }
    void visitOperandsCons( OperandsCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Order const& self ) {
  class Visitor : public Order::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitOrderOrder( OrderOrder const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, OrderByClause const& self ) {
  class Visitor : public OrderByClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitOrderByClauseEps( OrderByClauseEps const& host ) {
      *out_ << host;
    }
    void visitOrderByClauseOrderBy( OrderByClauseOrderBy const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Orders const& self ) {
  class Visitor : public Orders::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitOrdersLast( OrdersLast const& host ) {
      *out_ << host;
    }
    void visitOrdersCons( OrdersCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, PrimaryKey const& self ) {
  class Visitor : public PrimaryKey::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitPrimaryKeyEps( PrimaryKeyEps const& host ) {
      *out_ << host;
    }
    void visitPrimaryKeyPrimaryKey( PrimaryKeyPrimaryKey const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, RowValueConstructor const& self ) {
  class Visitor : public RowValueConstructor::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitRowValue( RowValue const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, SchemaName const& self ) {
  class Visitor : public SchemaName::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSchemaNameEps( SchemaNameEps const& host ) {
      *out_ << host;
    }
    void visitSchemaNameSchema( SchemaNameSchema const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, SelectExpression const& self ) {
  class Visitor : public SelectExpression::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSelectExpressionAny( SelectExpressionAny const& host ) {
      *out_ << host;
    }
    void visitSelectExpressionTerm( SelectExpressionTerm const& host ) {
      *out_ << host;
    }
    void visitSelectExpressionTermAs( SelectExpressionTermAs const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, SelectExpressions const& self ) {
  class Visitor : public SelectExpressions::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSelectExpressionsLast( SelectExpressionsLast const& host ) {
      *out_ << host;
    }
    void visitSelectExpressionsCons( SelectExpressionsCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, SelectModifier const& self ) {
  class Visitor : public SelectModifier::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSelectModifierEps( SelectModifierEps const& host ) {
      *out_ << host;
    }
    void visitSelectModifierDistinct( SelectModifierDistinct const& host ) {
      *out_ << host;
    }
    void visitSelectModifierAll( SelectModifierAll const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Summand const& self ) {
  class Visitor : public Summand::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitSummandFactor( SummandFactor const& host ) {
      *out_ << host;
    }
    void visitSummandPlus( SummandPlus const& host ) {
      *out_ << host;
    }
    void visitSummandMinus( SummandMinus const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, TableAlias const& self ) {
  class Visitor : public TableAlias::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitTableAliasEps( TableAliasEps const& host ) {
      *out_ << host;
    }
    void visitTableAliasAlias( TableAliasAlias const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, TableExpression const& self ) {
  class Visitor : public TableExpression::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitTable( Table const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Term const& self ) {
  class Visitor : public Term::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitTermValue( TermValue const& host ) {
      *out_ << host;
    }
    void visitTermBindParameter( TermBindParameter const& host ) {
      *out_ << host;
    }
    void visitTermCase( TermCase const& host ) {
      *out_ << host;
    }
    void visitTermCaseWhen( TermCaseWhen const& host ) {
      *out_ << host;
    }
    void visitTermParen( TermParen const& host ) {
      *out_ << host;
    }
    void visitTermColumnRef( TermColumnRef const& host ) {
      *out_ << host;
    }
    void visitTermRowValueConstructor( TermRowValueConstructor const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Terms const& self ) {
  class Visitor : public Terms::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitTermsLast( TermsLast const& host ) {
      *out_ << host;
    }
    void visitTermsCons( TermsCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, Value const& self ) {
  class Visitor : public Value::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitValueString( ValueString const& host ) {
      *out_ << host;
    }
    void visitValueBoolean( ValueBoolean const& host ) {
      *out_ << host;
    }
    void visitValueTrue( ValueTrue const& host ) {
      *out_ << host;
    }
    void visitValueFalse( ValueFalse const& host ) {
      *out_ << host;
    }
    void visitValueNumber( ValueNumber const& host ) {
      *out_ << host;
    }
    void visitValueNull( ValueNull const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, WhenClause const& self ) {
  class Visitor : public WhenClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitWhen( When const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, WhenClauses const& self ) {
  class Visitor : public WhenClauses::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitWhenClausesLast( WhenClausesLast const& host ) {
      *out_ << host;
    }
    void visitWhenClausesCons( WhenClausesCons const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}

std::ostream& operator <<( std::ostream &out, WhereClause const& self ) {
  class Visitor : public WhereClause::ConstVisitor {
  public:
    std::ostream* out_;
    Visitor( std::ostream& out ):out_( &out ){}
    void visitWhereClauseEps( WhereClauseEps const& host ) {
      *out_ << host;
    }
    void visitWhereClauseWhere( WhereClauseWhere const& host ) {
      *out_ << host;
    }
  } visitor( out );
  self.accept( visitor );
  return out;
}


std::ostream& operator <<( std::ostream& out, QuerySelect const& self ) {
  out << "QuerySelect("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ", "<< *std::get< 3 >( self ) << ", "<< *std::get< 4 >( self ) << ", "<< *std::get< 5 >( self ) << ", "<< *std::get< 6 >( self ) << ", "<< *std::get< 7 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AndConditionCondition const& self ) {
  out << "AndConditionCondition("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AndConditionAnd const& self ) {
  out << "AndConditionAnd("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AsEps const& self ) {
  out << "AsEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AsAs const& self ) {
  out << "AsAs(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AscDescEps const& self ) {
  out << "AscDescEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AscDescAsc const& self ) {
  out << "AscDescAsc(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, AscDescDesc const& self ) {
  out << "AscDescDesc(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BetweenConditionBetween const& self ) {
  out << "BetweenConditionBetween("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BetweenConditionNotBetween const& self ) {
  out << "BetweenConditionNotBetween("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BindParameterQuestion const& self ) {
  out << "BindParameterQuestion(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BindParameterRef const& self ) {
  out << "BindParameterRef("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Case const& self ) {
  out << "Case("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CaseWhen const& self ) {
  out << "CaseWhen("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ColumnDefinition const& self ) {
  out << "ColumnDefinition("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ", "<< *std::get< 3 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ColumnDefsLast const& self ) {
  out << "ColumnDefsLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ColumnDefsCons const& self ) {
  out << "ColumnDefsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Column const& self ) {
  out << "Column("<< *std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareNE const& self ) {
  out << "CompareNE(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareLE const& self ) {
  out << "CompareLE(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareGE const& self ) {
  out << "CompareGE(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareEQ const& self ) {
  out << "CompareEQ(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareLT const& self ) {
  out << "CompareLT(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CompareGT const& self ) {
  out << "CompareGT(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionCompare const& self ) {
  out << "ConditionCompare("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionInCondition const& self ) {
  out << "ConditionInCondition("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionLikeCondition const& self ) {
  out << "ConditionLikeCondition("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionBetweenCondition const& self ) {
  out << "ConditionBetweenCondition("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionIsNullCondition const& self ) {
  out << "ConditionIsNullCondition("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionNotExpression const& self ) {
  out << "ConditionNotExpression("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ConditionParen const& self ) {
  out << "ConditionParen("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, CharType const& self ) {
  out << "CharType("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, IntegerType const& self ) {
  out << "IntegerType(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, BooleanType const& self ) {
  out << "BooleanType(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FloatType const& self ) {
  out << "FloatType(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, DateType const& self ) {
  out << "DateType(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ElseClauseEps const& self ) {
  out << "ElseClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ElseClauseElse const& self ) {
  out << "ElseClauseElse("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ExpressionAndCondition const& self ) {
  out << "ExpressionAndCondition("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ExpressionOr const& self ) {
  out << "ExpressionOr("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ExpressionsLast const& self ) {
  out << "ExpressionsLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ExpressionsCons const& self ) {
  out << "ExpressionsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FactorTerm const& self ) {
  out << "FactorTerm("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FactorTimes const& self ) {
  out << "FactorTimes("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FactorDivides const& self ) {
  out << "FactorDivides("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FamilyNameEps const& self ) {
  out << "FamilyNameEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FamilyNameFamily const& self ) {
  out << "FamilyNameFamily("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, From const& self ) {
  out << "From("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, FromWithColumnDefs const& self ) {
  out << "FromWithColumnDefs("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, GroupByClauseEps const& self ) {
  out << "GroupByClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, GroupByClauseGroupBy const& self ) {
  out << "GroupByClauseGroupBy("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, HavingClauseEps const& self ) {
  out << "HavingClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, HavingClauseHaving const& self ) {
  out << "HavingClauseHaving("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, InConditionIn const& self ) {
  out << "InConditionIn("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, InConditionNotIn const& self ) {
  out << "InConditionNotIn("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, IsNullConditionIsNull const& self ) {
  out << "IsNullConditionIsNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, IsNullConditionIsNotNull const& self ) {
  out << "IsNullConditionIsNotNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, LikeConditionLike const& self ) {
  out << "LikeConditionLike("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, LikeConditionNotLike const& self ) {
  out << "LikeConditionNotLike("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, LimitClauseEps const& self ) {
  out << "LimitClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, LimitClauseLimitBindParameter const& self ) {
  out << "LimitClauseLimitBindParameter("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, LimitClauseLimitNumber const& self ) {
  out << "LimitClauseLimitNumber("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullabilityEps const& self ) {
  out << "NullabilityEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullabilityNull const& self ) {
  out << "NullabilityNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullabilityNotNull const& self ) {
  out << "NullabilityNotNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullsEps const& self ) {
  out << "NullsEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullsFirst const& self ) {
  out << "NullsFirst(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NullsLast const& self ) {
  out << "NullsLast(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, NumberInteger const& self ) {
  out << "NumberInteger("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OperandSummand const& self ) {
  out << "OperandSummand("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OperandAppend const& self ) {
  out << "OperandAppend("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OperandsLast const& self ) {
  out << "OperandsLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OperandsCons const& self ) {
  out << "OperandsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OrderOrder const& self ) {
  out << "OrderOrder("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OrderByClauseEps const& self ) {
  out << "OrderByClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OrderByClauseOrderBy const& self ) {
  out << "OrderByClauseOrderBy("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OrdersLast const& self ) {
  out << "OrdersLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, OrdersCons const& self ) {
  out << "OrdersCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, PrimaryKeyEps const& self ) {
  out << "PrimaryKeyEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, PrimaryKeyPrimaryKey const& self ) {
  out << "PrimaryKeyPrimaryKey("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, RowValue const& self ) {
  out << "RowValue("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SchemaNameEps const& self ) {
  out << "SchemaNameEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SchemaNameSchema const& self ) {
  out << "SchemaNameSchema("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectExpressionAny const& self ) {
  out << "SelectExpressionAny("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectExpressionTerm const& self ) {
  out << "SelectExpressionTerm("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectExpressionTermAs const& self ) {
  out << "SelectExpressionTermAs("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ", "<<  std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectExpressionsLast const& self ) {
  out << "SelectExpressionsLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectExpressionsCons const& self ) {
  out << "SelectExpressionsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectModifierEps const& self ) {
  out << "SelectModifierEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectModifierDistinct const& self ) {
  out << "SelectModifierDistinct(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SelectModifierAll const& self ) {
  out << "SelectModifierAll(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SummandFactor const& self ) {
  out << "SummandFactor("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SummandPlus const& self ) {
  out << "SummandPlus("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, SummandMinus const& self ) {
  out << "SummandMinus("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TableAliasEps const& self ) {
  out << "TableAliasEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TableAliasAlias const& self ) {
  out << "TableAliasAlias("<< *std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, Table const& self ) {
  out << "Table("<< *std::get< 0 >( self ) << ", "<<  std::get< 1 >( self ) << ", "<< *std::get< 2 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermValue const& self ) {
  out << "TermValue("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermBindParameter const& self ) {
  out << "TermBindParameter("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermCase const& self ) {
  out << "TermCase("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermCaseWhen const& self ) {
  out << "TermCaseWhen("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermParen const& self ) {
  out << "TermParen("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermColumnRef const& self ) {
  out << "TermColumnRef("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermRowValueConstructor const& self ) {
  out << "TermRowValueConstructor("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermsLast const& self ) {
  out << "TermsLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, TermsCons const& self ) {
  out << "TermsCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueString const& self ) {
  out << "ValueString("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueBoolean const& self ) {
  out << "ValueBoolean("<<  std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueTrue const& self ) {
  out << "ValueTrue(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueFalse const& self ) {
  out << "ValueFalse(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueNumber const& self ) {
  out << "ValueNumber("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, ValueNull const& self ) {
  out << "ValueNull(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, When const& self ) {
  out << "When("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, WhenClausesLast const& self ) {
  out << "WhenClausesLast("<< *std::get< 0 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, WhenClausesCons const& self ) {
  out << "WhenClausesCons("<< *std::get< 0 >( self ) << ", "<< *std::get< 1 >( self ) << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, WhereClauseEps const& self ) {
  out << "WhereClauseEps(" << ")";
  return out;
}

std::ostream& operator <<( std::ostream& out, WhereClauseWhere const& self ) {
  out << "WhereClauseWhere("<< *std::get< 0 >( self ) << ")";
  return out;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Node1::Node1() {}

Node2::Node2( std::shared_ptr< Query > const& arg1_)
    :arg1( arg1_ ) {}

Node3::Node3( std::shared_ptr< AndCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node4::Node4() {}

Node5::Node5() {}

Node6::Node6() {}

Node7::Node7() {}

Node8::Node8() {}

Node9::Node9() {}

Node10::Node10() {}

Node11::Node11() {}

Node12::Node12() {}

Node13::Node13() {}

Node14::Node14() {}

Node15::Node15() {}

Node16::Node16( std::shared_ptr< Condition > const& arg1_)
    :arg1( arg1_ ) {}

Node17::Node17() {}

Node18::Node18( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node19::Node19( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node20::Node20() {}

Node21::Node21() {}

Node22::Node22( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node23::Node23() {}

Node24::Node24( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node25::Node25() {}

Node26::Node26() {}

Node27::Node27( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node28::Node28( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node29::Node29( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node30::Node30( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node31::Node31() {}

Node32::Node32() {}

Node33::Node33( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node34::Node34() {}

Node35::Node35() {}

Node36::Node36( std::shared_ptr< SelectModifier > const& arg1_)
    :arg1( arg1_ ) {}

Node37::Node37() {}

Node38::Node38() {}

Node39::Node39() {}

Node40::Node40() {}

Node41::Node41() {}

Node42::Node42() {}

Node43::Node43( std::shared_ptr< Compare > const& arg1_)
    :arg1( arg1_ ) {}

Node44::Node44() {}

Node45::Node45() {}

Node46::Node46() {}

Node47::Node47() {}

Node48::Node48() {}

Node49::Node49() {}

Node50::Node50() {}

Node51::Node51() {}

Node52::Node52() {}

Node53::Node53() {}

Node54::Node54() {}

Node55::Node55() {}

Node56::Node56( int const& arg1_)
    :arg1( arg1_ ) {}

Node57::Node57() {}

Node58::Node58( std::shared_ptr< ColumnRef > const& arg1_)
    :arg1( arg1_ ) {}

Node59::Node59() {}

Node60::Node60( std::shared_ptr< WhenClauses > const& arg1_)
    :arg1( arg1_ ) {}

Node61::Node61( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node62::Node62( std::shared_ptr< ElseClause > const& arg1_)
    :arg1( arg1_ ) {}

Node63::Node63() {}

Node64::Node64( std::shared_ptr< WhenClauses > const& arg1_)
    :arg1( arg1_ ) {}

Node65::Node65( std::shared_ptr< ElseClause > const& arg1_)
    :arg1( arg1_ ) {}

Node66::Node66( int const& arg1_)
    :arg1( arg1_ ) {}

Node67::Node67( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node68::Node68() {}

Node69::Node69( std::shared_ptr< TableExpression > const& arg1_)
    :arg1( arg1_ ) {}

Node70::Node70( std::shared_ptr< FamilyName > const& arg1_)
    :arg1( arg1_ ) {}

Node71::Node71( std::shared_ptr< FamilyName > const& arg1_)
    :arg1( arg1_ ) {}

Node72::Node72( std::shared_ptr< PrimaryKey > const& arg1_)
    :arg1( arg1_ ) {}

Node73::Node73( std::shared_ptr< DataType > const& arg1_)
    :arg1( arg1_ ) {}

Node74::Node74( std::shared_ptr< Nullability > const& arg1_)
    :arg1( arg1_ ) {}

Node75::Node75( std::shared_ptr< ColumnDefs > const& arg1_)
    :arg1( arg1_ ) {}

Node76::Node76( std::shared_ptr< ColumnDef > const& arg1_)
    :arg1( arg1_ ) {}

Node77::Node77() {}

Node78::Node78() {}

Node79::Node79() {}

Node80::Node80() {}

Node81::Node81() {}

Node82::Node82() {}

Node83::Node83( std::shared_ptr< BetweenCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node84::Node84( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node85::Node85( std::shared_ptr< InCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node86::Node86( std::shared_ptr< IsNullCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node87::Node87( std::shared_ptr< LikeCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node88::Node88( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node89::Node89() {}

Node90::Node90( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node91::Node91() {}

Node92::Node92( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node93::Node93( std::shared_ptr< AndCondition > const& arg1_)
    :arg1( arg1_ ) {}

Node94::Node94( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node95::Node95( std::shared_ptr< Expressions > const& arg1_)
    :arg1( arg1_ ) {}

Node96::Node96( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node97::Node97( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node98::Node98( std::shared_ptr< Factor > const& arg1_)
    :arg1( arg1_ ) {}

Node99::Node99( std::shared_ptr< Factor > const& arg1_)
    :arg1( arg1_ ) {}

Node100::Node100( std::shared_ptr< Factor > const& arg1_)
    :arg1( arg1_ ) {}

Node101::Node101( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node102::Node102( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node103::Node103() {}

Node104::Node104( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node105::Node105() {}

Node106::Node106() {}

Node107::Node107( std::shared_ptr< SelectExpressions > const& arg1_)
    :arg1( arg1_ ) {}

Node108::Node108( std::shared_ptr< ColumnDefs > const& arg1_)
    :arg1( arg1_ ) {}

Node109::Node109( std::shared_ptr< WhereClause > const& arg1_)
    :arg1( arg1_ ) {}

Node110::Node110( std::shared_ptr< Expressions > const& arg1_)
    :arg1( arg1_ ) {}

Node111::Node111() {}

Node112::Node112( std::shared_ptr< GroupByClause > const& arg1_)
    :arg1( arg1_ ) {}

Node113::Node113( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node114::Node114( std::shared_ptr< Operands > const& arg1_)
    :arg1( arg1_ ) {}

Node115::Node115( std::shared_ptr< Operands > const& arg1_)
    :arg1( arg1_ ) {}

Node116::Node116() {}

Node117::Node117() {}

Node118::Node118() {}

Node119::Node119() {}

Node120::Node120() {}

Node121::Node121( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node122::Node122( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node123::Node123( std::shared_ptr< OrderByClause > const& arg1_)
    :arg1( arg1_ ) {}

Node124::Node124( std::shared_ptr< BindParameter > const& arg1_)
    :arg1( arg1_ ) {}

Node125::Node125( std::shared_ptr< Number > const& arg1_)
    :arg1( arg1_ ) {}

Node126::Node126() {}

Node127::Node127() {}

Node128::Node128() {}

Node129::Node129( std::shared_ptr< AscDesc > const& arg1_)
    :arg1( arg1_ ) {}

Node130::Node130() {}

Node131::Node131() {}

Node132::Node132() {}

Node133::Node133( int const& arg1_)
    :arg1( arg1_ ) {}

Node134::Node134( std::shared_ptr< Summand > const& arg1_)
    :arg1( arg1_ ) {}

Node135::Node135( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node136::Node136( std::shared_ptr< Operand > const& arg1_)
    :arg1( arg1_ ) {}

Node137::Node137( std::shared_ptr< Summand > const& arg1_)
    :arg1( arg1_ ) {}

Node138::Node138( std::shared_ptr< Operands > const& arg1_)
    :arg1( arg1_ ) {}

Node139::Node139( std::shared_ptr< HavingClause > const& arg1_)
    :arg1( arg1_ ) {}

Node140::Node140( std::shared_ptr< Orders > const& arg1_)
    :arg1( arg1_ ) {}

Node141::Node141() {}

Node142::Node142( std::shared_ptr< Nulls > const& arg1_)
    :arg1( arg1_ ) {}

Node143::Node143( std::shared_ptr< Orders > const& arg1_)
    :arg1( arg1_ ) {}

Node144::Node144( std::shared_ptr< Order > const& arg1_)
    :arg1( arg1_ ) {}

Node145::Node145( std::shared_ptr< AscDesc > const& arg1_)
    :arg1( arg1_ ) {}

Node146::Node146() {}

Node147::Node147( std::shared_ptr< LimitClause > const& arg1_)
    :arg1( arg1_ ) {}

Node148::Node148() {}

Node149::Node149( std::shared_ptr< FromClause > const& arg1_)
    :arg1( arg1_ ) {}

Node150::Node150() {}

Node151::Node151( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node152::Node152( std::shared_ptr< Terms > const& arg1_)
    :arg1( arg1_ ) {}

Node153::Node153() {}

Node154::Node154( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node155::Node155() {}

Node156::Node156( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node157::Node157( std::shared_ptr< As > const& arg1_)
    :arg1( arg1_ ) {}

Node158::Node158( std::shared_ptr< SelectExpressions > const& arg1_)
    :arg1( arg1_ ) {}

Node159::Node159( std::shared_ptr< SelectExpression > const& arg1_)
    :arg1( arg1_ ) {}

Node160::Node160() {}

Node161::Node161() {}

Node162::Node162( std::shared_ptr< TableAlias > const& arg1_)
    :arg1( arg1_ ) {}

Node163::Node163( std::shared_ptr< SchemaName > const& arg1_)
    :arg1( arg1_ ) {}

Node164::Node164( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node165::Node165( std::shared_ptr< As > const& arg1_)
    :arg1( arg1_ ) {}

Node166::Node166( std::shared_ptr< BindParameter > const& arg1_)
    :arg1( arg1_ ) {}

Node167::Node167( std::shared_ptr< CaseClause > const& arg1_)
    :arg1( arg1_ ) {}

Node168::Node168( std::shared_ptr< CaseWhenClause > const& arg1_)
    :arg1( arg1_ ) {}

Node169::Node169( std::shared_ptr< ColumnRef > const& arg1_)
    :arg1( arg1_ ) {}

Node170::Node170() {}

Node171::Node171( std::shared_ptr< RowValueConstructor > const& arg1_)
    :arg1( arg1_ ) {}

Node172::Node172( std::shared_ptr< Value > const& arg1_)
    :arg1( arg1_ ) {}

Node173::Node173( std::shared_ptr< Terms > const& arg1_)
    :arg1( arg1_ ) {}

Node174::Node174( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node175::Node175( bool const& arg1_)
    :arg1( arg1_ ) {}

Node176::Node176() {}

Node177::Node177() {}

Node178::Node178( std::shared_ptr< Number > const& arg1_)
    :arg1( arg1_ ) {}

Node179::Node179( std::string const& arg1_)
    :arg1( arg1_ ) {}

Node180::Node180() {}

Node181::Node181( std::shared_ptr< Term > const& arg1_)
    :arg1( arg1_ ) {}

Node182::Node182( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

Node183::Node183( std::shared_ptr< WhenClause > const& arg1_)
    :arg1( arg1_ ) {}

Node184::Node184( std::shared_ptr< WhenClauses > const& arg1_)
    :arg1( arg1_ ) {}

Node185::Node185( std::shared_ptr< Expression > const& arg1_)
    :arg1( arg1_ ) {}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

std::shared_ptr< State< Node1 > > begin() {
  std::shared_ptr< State<> > bottom( new State<>() );
  return State< Node1 >::make( Node1(), bottom );
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

}

