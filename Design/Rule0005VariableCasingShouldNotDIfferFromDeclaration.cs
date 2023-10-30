using Microsoft.Dynamics.Nav.CodeAnalysis;
using Microsoft.Dynamics.Nav.CodeAnalysis.Diagnostics;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace BusinessCentral.LinterCop.Design
{
    [DiagnosticAnalyzer]
    public class Rule0005VariableCasingShouldNotDIfferFromDeclaration : DiagnosticAnalyzer
    {
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create<DiagnosticDescriptor>(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration);

        public override void Initialize(AnalysisContext context)
        {
            context.RegisterOperationAction(new Action<OperationAnalysisContext>(this.CheckForBuiltInMethodsWithCasingMismatch), new OperationKind[] {
                OperationKind.InvocationExpression,
                OperationKind.FieldAccess,
                OperationKind.GlobalReferenceExpression,
                OperationKind.LocalReferenceExpression,
                OperationKind.ParameterReferenceExpression,
                OperationKind.ReturnValueReferenceExpression
            });

            context.RegisterSymbolAction(new Action<SymbolAnalysisContext>(this.CheckForBuiltInTypeCasingMismatch), new SymbolKind[] {
                SymbolKind.Codeunit,
                SymbolKind.Entitlement,
                SymbolKind.Enum,
                SymbolKind.EnumExtension,
                SymbolKind.Interface,
                SymbolKind.Page,
                SymbolKind.PageCustomization,
                SymbolKind.PageExtension,
                SymbolKind.Permission,
                SymbolKind.PermissionSet,
                SymbolKind.PermissionSetExtension,
                SymbolKind.Profile,
                SymbolKind.ProfileExtension,
                SymbolKind.Query,
                SymbolKind.Report,
                SymbolKind.ReportExtension,
                SymbolKind.Table,
                SymbolKind.TableExtension,
                SymbolKind.XmlPort
            });
        }

        private void CheckForBuiltInTypeCasingMismatch(SymbolAnalysisContext ctx)
        {
            foreach (Microsoft.Dynamics.Nav.CodeAnalysis.Syntax.SyntaxNodeOrToken node in ctx.Symbol.DeclaringSyntaxReference.GetSyntax().DescendantNodesAndTokens().Where(currentNode => IsValidToken(currentNode)))
            {
                if (node.Kind.ToString().StartsWith("DotNet"))
                {
                    continue;
                }

                if (node.IsToken)
                    if (SyntaxFactory.Token(node.Kind).ToString() != node.ToString())
                        ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, node.GetLocation(), new object[] { SyntaxFactory.Token(node.Kind), "" }));

                if (node.IsNode && !node.AsNode().ToString().StartsWith("array"))
                {
                    if ((node.AsNode().IsKind(SyntaxKind.SimpleTypeReference) || node.Kind.ToString().Contains("DataType")) && !node.Kind.ToString().StartsWith("Codeunit") && !node.Kind.ToString().StartsWith("Enum") && !node.Kind.ToString().StartsWith("Label"))
                    {
                        NavTypeKind targetName;

                        // node parse results in analyzer crashes??
                        if (Enum.TryParse(node.AsNode().ToString(), true, out targetName) && targetName.ToString().Equals(node.Kind.ToString(), StringComparison.CurrentCultureIgnoreCase))
                        {
                            ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, node.GetLocation(), new object[] { targetName, "" }));
                        }
                        // var targetName = Enum.GetValues(typeof(NavTypeKind)).Cast<NavTypeKind>().FirstOrDefault(Kind => Kind.ToString().Equals(node.AsNode().ToString(),StringComparison.CurrentCultureIgnoreCase));
                        // if (targetName != NavTypeKind.None)
                        //     ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, node.GetLocation(), new object[] { targetName, "" }));
                    }
                    if (node.AsNode().IsKind(SyntaxKind.SubtypedDataType) || node.AsNode().IsKind(SyntaxKind.GenericDataType) || node.AsNode().IsKind(SyntaxKind.OptionAccessExpression) ||
                       (node.AsNode().IsKind(SyntaxKind.SimpleTypeReference) && (node.Kind.ToString().StartsWith("Codeunit") || !node.Kind.ToString().StartsWith("Enum") || !node.Kind.ToString().StartsWith("Label"))))
                    {
                        NavTypeKind targetName = Enum.GetValues(typeof(NavTypeKind)).Cast<NavTypeKind>().FirstOrDefault(Kind => node.AsNode().ToString().ToUpper().StartsWith(Kind.ToString().ToUpper()) && !node.AsNode().ToString().StartsWith(Kind.ToString()));
                        if (targetName != NavTypeKind.None)
                            ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, node.GetLocation(), new object[] { targetName, "" }));
                    }
                }
            }
        }

        private static bool IsValidToken(Microsoft.Dynamics.Nav.CodeAnalysis.Syntax.SyntaxNodeOrToken node)
        {
            string nodeKindName = node.Kind.ToString();

            if (new[] {SyntaxKind.SimpleTypeReference,
                SyntaxKind.SubtypedDataType,
                SyntaxKind.GenericDataType,
                SyntaxKind.OptionAccessExpression}.Contains(node.Kind)) return true;

            string[] validDataTypes = new[] { // minor improvement??
                            "Codeunit",
                            "Enum",
                            "Label",
                            "Action",
                            "Page",
                            "Interface",
                            "Report",
                            "Query",
                            "XmlPort",
                            "DotNet"
                };

            if (nodeKindName.Contains("Keyword"))
            {
                if (!validDataTypes.Contains(nodeKindName)) return true;
            }

            if (nodeKindName.Contains("DataType"))
                return true;

            return false;
        }

        private void CheckForBuiltInMethodsWithCasingMismatch(OperationAnalysisContext ctx)
        {
            if (ctx.ContainingSymbol.GetContainingObjectTypeSymbol().IsObsoletePending || ctx.ContainingSymbol.GetContainingObjectTypeSymbol().IsObsoleteRemoved) return;
            if (ctx.ContainingSymbol.IsObsoletePending || ctx.ContainingSymbol.IsObsoleteRemoved) return;

            var targetName = "";
            if (ctx.Operation.Kind == OperationKind.InvocationExpression)
            {
                IInvocationExpression operation = (IInvocationExpression)ctx.Operation;
                targetName = operation.TargetMethod.Name;
            }
            if (ctx.Operation.Kind == OperationKind.FieldAccess)
            {
                try
                {
                    IFieldAccess operation = (IFieldAccess)ctx.Operation;
                    targetName = operation.FieldSymbol.Name;
                }
                catch (System.InvalidCastException)
                {
                }
            }

            if (new object[] {
                OperationKind.GlobalReferenceExpression,
                OperationKind.LocalReferenceExpression,
                OperationKind.ParameterReferenceExpression,
                OperationKind.ReturnValueReferenceExpression }.Contains(ctx.Operation.Kind))
            {
                switch (ctx.Operation.Kind)
                {
                    case OperationKind.GlobalReferenceExpression:
                        targetName = ((IGlobalReferenceExpression)ctx.Operation).GlobalVariable.Name;
                        break;
                    case OperationKind.LocalReferenceExpression:
                        targetName = ((ILocalReferenceExpression)ctx.Operation).LocalVariable.Name;
                        break;
                    case OperationKind.ParameterReferenceExpression:
                        targetName = ((IParameterReferenceExpression)ctx.Operation).Parameter.Name;
                        break;
                    case OperationKind.ReturnValueReferenceExpression:
                        targetName = ((IReturnValueReferenceExpression)ctx.Operation).ReturnValue.Name;
                        break;
                }
            }

            if (OnlyDiffersInCasing(ctx.Operation.Syntax.ToString(), targetName))
            {
                ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, ctx.Operation.Syntax.GetLocation(), new object[] { targetName, "" }));
                return;
            }

            // var nodes = ctx.Operation.Syntax.DescendantNodes(nodes => OnlyDiffersInCasing(nodes.ToString(), targetName));
            var nodes = Array.Find(ctx.Operation.Syntax.DescendantNodes((SyntaxNode e) => true).ToArray(), element => OnlyDiffersInCasing(element.ToString(), targetName));
            if (nodes != null)
                ctx.ReportDiagnostic(Diagnostic.Create(DiagnosticDescriptors.Rule0005VariableCasingShouldNotDIfferFromDeclaration, ctx.Operation.Syntax.GetLocation(), new object[] { targetName, "" }));
        }
        private bool OnlyDiffersInCasing(string left, string right)
        {
            return left.Trim('"').ToUpper() == right.Trim('"').ToUpper() && left.Trim('"') != right.Trim('"');
        }
    }
}
