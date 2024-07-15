﻿using BusinessCentral.LinterCop.AnalysisContextExtension;
using Microsoft.Dynamics.Nav.CodeAnalysis;
using Microsoft.Dynamics.Nav.CodeAnalysis.Diagnostics;
using System.Collections.Immutable;

namespace BusinessCentral.LinterCop.Design
{
    [DiagnosticAnalyzer]
    public class Rule0006FieldNotAutoIncrementInTemporaryTable : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create<DiagnosticDescriptor>(DiagnosticDescriptors.Rule0006FieldNotAutoIncrementInTemporaryTable);

        public override void Initialize(AnalysisContext context)
            => context.RegisterSymbolAction(new Action<SymbolAnalysisContext>(this.CheckTablePrimaryKeyIsNotAutoIncrement), SymbolKind.Table);

        private void CheckTablePrimaryKeyIsNotAutoIncrement(SymbolAnalysisContext context)
        {
            if (context.IsObsoletePendingOrRemoved()) return;
            ITableTypeSymbol tableTypeSymbol = (ITableTypeSymbol)context.Symbol;
            if (!IsSymbolAccessible(tableTypeSymbol))
                return;

            CheckTable(tableTypeSymbol, ref context);
        }

        private void CheckTable(ITableTypeSymbol table, ref SymbolAnalysisContext context)
        {
            if (table.TableType != TableTypeKind.Temporary)
                return;

            foreach (var field in table.Fields)
            {
                IPropertySymbol propertySymbol = field.GetProperty(PropertyKind.AutoIncrement);
                if (propertySymbol == null)
                    continue;

                if (propertySymbol?.ValueText != "0")
                {
                    context.ReportDiagnostic(
                        Diagnostic.Create(
                            DiagnosticDescriptors.Rule0006FieldNotAutoIncrementInTemporaryTable,
                            propertySymbol.GetLocation()));
                }
            }
        }

        private static string GetDeclaration(ISymbol symbol)
            => symbol.Location.SourceTree.GetText(CancellationToken.None).GetSubText(symbol.DeclaringSyntaxReference.Span).ToString();

        private static bool IsSymbolAccessible(ISymbol symbol)
        {
            try
            {
                GetDeclaration(symbol);
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }
    }

}
