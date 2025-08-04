Analyze the following %s code and generate comprehensive documentation:

**File:** %s
**Language:** %s

**Code:**
`+"```"+`%s
%s
`+"```"+`

Please provide:
1. **Overview**: Brief description of the file's purpose
2. **Functions/Classes**: Document each function/class with:
   - Purpose and functionality
   - Parameters (name, type, description)
   - Return values (type, description)
   - Usage examples where helpful
3. **Dependencies**: List and explain imported modules/libraries
4. **Architecture Notes**: How this fits into the larger system

Format the response as clean, professional documentation that would be helpful for other developers.
`, language, filename, language, language, code)
}

func (c *QwenClient) getSystemPrompt() string {
	return `You are an expert software documentation specialist with deep knowledge of multiple programming languages including TypeScript, JavaScript, Python, C#, and COBOL. 

Your role is to analyze code and generate clear, comprehensive, and professional documentation that helps developers understand:
- What the code does (functionality)
- How to use it (interface/API)
- Why design decisions were made (context)
- How it fits into the larger system (architecture)

Always write documentation that is:
- Clear and concise
- Technically accurate
- Professional in tone
- Focused on practical usage
- Well-structured and easy to read

Include concrete examples when they add value, and explain complex concepts in accessible terms.`